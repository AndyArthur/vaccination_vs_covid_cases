library(tidyverse)
library(tigris)
library(openxlsx)
library(sf)
library(janitor)

fipscode = '09'

df <- read.xlsx(str_c('https://www2.census.gov/programs-surveys/popest/tables/2020-2021/counties/totals/co-est2021-pop-',fipscode,'.xlsx'))
df <- janitor::row_to_names(df, 2) %>% clean_names() %>% drop_na()

df <- df %>% rename('2021_pop'='na', '2020_pop'= 'population_estimate_as_of_july_1' , 
              '2020_base'='april_1_2020_estimates_base') %>%
  mutate(geographic_area = str_replace(geographic_area,'^\\.','')) %>%
  separate(`geographic_area`, c('County','State'), sep=',') %>% drop_na()

df <- type_convert(df)

df <- df %>% mutate(`Pop_Chg` = (((`2021_pop`-`2020_pop`)/`2020_pop`)*100))

nyco <- counties(state=fipscode,cb = T, resolution = '500k')

nyco <- nyco %>% inner_join(df, by=c('NAMELSAD'='County')) 

countiesmerge <- st_sf(st_union(nyco))

ggplot(nyco) + geom_sf(aes(fill=Pop_Chg), size=0.3) + 
  scale_fill_gradient2(midpoint = 0, breaks = seq(-10,10,1)) +
  ggfx::with_outer_glow(geom_sf(data=countiesmerge, fill=NA, size=0.5),col="darkgray",signma=10 )+
  ggsflabel::geom_sf_label_repel(aes(label=str_c(scales::comma(Pop_Chg, accuracy = 0.1),'%')), 
                                 point.size = NA, size=3, label.padding = unit(0.15, "lines"),
                                 min.segment.length =0, box.padding = 0.5
                                 ) +
  labs(title = 'Population Change - July 2020 to 2021',
   #    subtitle = 'During the era of remote work and the panademic, Pennsylvania saw significant growth in the Poconos.',
       y = "",
       x = "",
      caption='https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%-d/%y")),
       fill = "") +
  theme_void() +
  coord_sf(expand=F) +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5, margin=margin(t=10, b=10)),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'None',
  )

filename <- str_c(fipscode,'_county_pop_change')
height <- 1800
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=130)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=130, device = grDevices::svg)

system(paste('scour /tmp/',filename,'.svg', ' /tmp/',filename,'.svgz',sep=''))  
