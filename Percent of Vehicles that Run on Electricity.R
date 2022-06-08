library(data.table)
library(tigris)
library(tidyverse)
library(sf)
rm(list=ls())

auto <- fread(cmd='unzip -p /media/hd2/draft.redistricting/autoreg.csv.zip')

elect <- auto[`Record Type` == 'VEH' & State == 'NY', .(.N), by=.(Zip, `Fuel Type`)]
elect$Zip <- as.character(elect$Zip)

elect <- elect %>% pivot_wider(names_from = `Fuel Type`, values_from = 'N') %>% mutate(elper = (ELECTRIC/rowSums(across(-Zip), na.rm=T))*100)

zcta <- zctas(cb=T, year=2019) 

zcta %>% right_join(elect, by=c('ZCTA5CE10'='Zip')) %>% write_sf('/tmp/electric.gpkg')

zipc <- read_csv('Documents/GIS.Data/ZipCode/zip-code-data.csv')

zipc$GEOID <- 
  str_pad(zipc$state_fips,side = 'left',pad = '0', width = 2) %>% 
  str_c(zipc$zipcode)

electzip <- zcta %>% 
  inner_join(elect, by=c('ZCTA5CE10'='Zip')) %>% inner_join(zipc, by=c('ZCTA5CE10'='zipcode')) %>%
  filter(state_abbr == 'NY') %>% mutate_all(~replace(., is.na(.), 0))

library(classInt)

classes <- classIntervals(electzip$elper, n = 8, style = "fisher", dataPercision=0)

electzip <- electzip %>% mutate(percent_class = cut(electzip$elper, breaks=classes$brks, include.lowest = T, 
                                                labels=scales::comma(classes$brks[2:length(classes$brks)],0.1)) )

#electzip <- electzip %>% st_intersection(st_union(co))

library(ggtext)

ggplot(electzip) + 
  geom_sf(aes(fill=percent_class),size=0) +
  geom_sf(data=co, fill=NA, size=0.4) +
  scale_fill_brewer(palette = 'greens', name='Percent') +
  labs(title = 'Percent of <span style="font-size: 30pt; color: darkgreen">Registered Motor Vehicles</span> that run on <span style="color: darkgreen; font-size: 30pt">Electricity</span>',
       y = "",
       x = "",
       caption='NYS DMV, Active Registations on May 1, 2022',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(0,0,3,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'top',
  ) +
  coord_sf(expand=F) 


bounce <- co %>% st_transform(3857) %>% st_bbox()
bounce <- (abs(bounce[4]-bounce[2])/abs(bounce[3]-bounce[1]))+0.05


fn <- str_c('most-electric')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1920*bounce, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1920*bounce, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

