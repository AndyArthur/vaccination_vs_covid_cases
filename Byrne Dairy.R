library(tidyverse)
library(sf)
library(tigris)
library(ggtext)
library(units)

rm(list=ls())

rf <- read_sf('https://data.ny.gov/api/geospatial/9a8c-vfzj?accessType=DOWNLOAD&method=export&format=GeoJSON')

nyco <- counties('ny', cb=T) %>% st_transform(3857)

rf %>% mutate(square_footage = parse_number(square_footage)) %>% 
  filter(entity_name == 'SONBYRNE SALES INC') -> sws 

swss <- sws %>% st_transform(3857) %>% st_union() %>% st_convex_hull() %>% st_buffer(set_units(10,'mi')) %>%
  st_intersection(nyco)

ggplot(swss) + geom_sf(fill='yellow') + 
  geom_sf(data=sws, color='red') +
  geom_sf(data=nyco, fill=NA, linewidth=0.4) +
  theme_void() +
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: red; font-size: 90pt">Byrne Dairy</span> in New York State'),
       y = "",
       x = "",
       tag = paste('<span style="color: gray20; font-size: 36pt">Byrne Dairy is concentrated in Central New York, especially Syracuse and Rochester.</span><br /><span style="font-size:15pt">&nbsp;</span><br />',
                   'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br />Source: NYS Ag & Markets Grocery Stores. data.ny.gov/api/geospatial/9a8c-vfzj'),
       fill = "")  +
  theme(
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=46, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555', width=0.7, valign=0, vjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(2.5,'cm'),
    legend.key.width = unit(1.5,'cm'),
    legend.position = c(0.92,0.6),
  ) 

fn <- str_c('byrne')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

