library(tidyverse)
library(tigris)
library(tidycensus)
library(zoo)
library(sf)
library(classInt)

covid <- read_csv('https://health.data.ny.gov/api/views/kmxh-hz9i/rows.csv?accessType=DOWNLOAD')
covid$`Report Date`  <- as.Date(covid$`Report Date`, format = "%m/%d/%Y")

# get a list of counties and zip codes for aggregation
zip2county <- read_csv('https://data.ny.gov/api/views/juva-r6g2/rows.csv?accessType=DOWNLOAD')

mostRecent <- covid %>% filter(covid$`Report Date` > max(covid$`Report Date`)-8)
mostRecent$zip <- mostRecent$`Zip Code` %>% str_pad(5, pad = "0")

countypop <- get_decennial(
  geography = "county",
  state = 'ny',
  variables = "P1_001N",
  year = 2020,
  sumfile = "pl"
)

mostRecent <- mostRecent %>% inner_join(zip2county, by=c('zip'='ZIP Code')) %>% group_by(`County Code`) %>% summarise('hospTotal' = sum(`Number of Admissions`)) %>%
  mutate('GEOID' = paste('36', `County Code`,sep='')) %>%  inner_join(countypop, by=c('GEOID')) %>% mutate('per100k'=(hospTotal/value)*100000)

classes <- classIntervals(mostRecent$per100k, n = 11, style = "quantile", na.rm=T)
mostRecent <- mostRecent %>% mutate(percent_class = cut(mostRecent$per100k, breaks=classes$brks, include.lowest = T,
                                                        labels=round(classes$brks[1:length(classes$brks)-1],0)
))

cosub <- county_subdivisions(state='ny', cb=T) %>% st_simplify(preserveTopology = TRUE, dTolerance = 1000)
counties <- counties(state='ny', cb=T) %>% inner_join(mostRecent, by=c('GEOID')) %>% st_simplify(preserveTopology = TRUE, dTolerance = 1000)


ggplot(counties, aes(fill=percent_class)) + geom_sf(size=0.01) + 
  scale_fill_viridis_d(option='D', name='COVID Hospitalizations\nper 100k residents') +
  geom_sf(data=cosub, size=0.1, color='lightgray', fill=NA)+
  geom_sf(data=counties, size=0.5, color='lightgray', fill=NA)+
  ggsflabel::geom_sf_label_repel(aes(label=scales::comma(per100k,accuracy=0.1)),size=3, point.size = NA, fill='white', label.padding = unit(0.2, "lines"))+
  labs(title =paste('COVID-19 Hospitalizations per 100,000 residents,\n 7-days prior to', format((max(covid$`Report Date`)+1),'%B %-d, %Y')),
       #subtitle='Western NY, Tug Hill and Mohawk Valley have high rates of COVID-19 as we head into the holiday weekend',
       y = "",
       x = "",
       caption='NYS Health Department, data.health.ny.gov',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(4,'cm'),
    legend.key.size = unit(0.25,'cm'),
    legend.position = 'bottom'
  )


ggsave(paste('/tmp/covid-county-hosp.jpg',sep=''), width=1920, height=1600, units='px', dpi=150)
ggsave(paste('/tmp/covid-county-hosp.svg',sep=''), width=1920, height=1600, units='px', dpi=150, device = grDevices::svg)

for (countyname in list_counties('ny')$county) {
  bbox <- counties(state='ny') %>% filter(NAME == countyname)  %>% st_buffer(2000) %>% st_bbox()
  
  county <- counties(state='ny', cb=TRUE) %>% st_crop(bbox)
  cosub <- county_subdivisions(state='ny')%>% st_crop(bbox)
  #zcatpart <- zcat  %>% st_crop(bbox)
  
  title <- paste('December 21-28, 2021\nNew COVID Cases\nper 10k residents in ',countyname,'County');
  
  ggplot(zcat, aes()) + geom_sf(aes(fill=percent_class), size=0) +  
    scale_fill_viridis_d(option='C', name='New Positive Cases,\nPer 10k Residents') +
    geom_sf(data=cosub, size=0.15, color='white', fill=NA)+  
    ggsflabel::geom_sf_text_repel(data=cosub, color='white',fontface='bold',point.size=NA, mapping=aes(label=stringr::str_wrap(NAME,10)), size=3)+  
    geom_sf(data=county, size=0.8, color='white', fill=NA)+ 
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    coord_sf(xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4])) +
    labs(
      title=title,
      tag='Andy Arthur, 12/28/2021',
      caption='NYS Health Department, data.health.ny.gov'
    ) +
    theme_void() + # dark theme
    theme(
      text= element_text(family='Overpass',size=14, color="black"),
      plot.title=element_text(hjust=0.5, face='bold',size=30),
      plot.background = element_rect(fill = "White", size=0),
      panel.background = element_rect(fill = "White", size=0),
      plot.subtitle=element_text(hjust=0.5),
      plot.tag=element_text(size=10,hjust=0, color='black'),
      plot.caption=element_text(size=10, color='black'),
      plot.margin = unit(c(1,1,1,1), 'lines'),
      plot.tag.position = c(0.0,0.01),
      legend.key.width = unit(3,'cm'),
      legend.position = 'bottom',
    )
  
  ggsave(paste('Desktop/covidzip/',countyname,'.jpg', sep=''), width=1920, height=1600, units='px', dpi=150)
  
}
