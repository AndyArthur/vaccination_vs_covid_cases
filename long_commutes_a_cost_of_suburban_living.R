library(tidycensus)
library(sf)
library(tidyverse)
library(tigris)

#acs <- load_variables(2019, "acs5/subject", cache=T)

bbox <-  county_subdivisions(state='ny') %>% filter(NAME %in% c('Albany','Clifton Park', 'Guilderland','North Greenbush','New Scotland','Bethlehem','Sand Lake')) %>% st_bbox()
county <- counties(state='ny', cb=TRUE) %>% st_crop(bbox)
cosub <- county_subdivisions(state='ny')  %>% st_crop(bbox)
label <- cosub %>% st_crop(bbox) 
travel<-get_acs(geography='tract',variables='S0801_C01_046',state='ny',geometry = TRUE, output='wide', cache_table=T) %>% st_crop(bbox)

title <- 'LONG COMMUTES: A cost of suburban living'

ggplot(travel, aes()) + geom_sf(aes(fill=S0801_C01_046E), size=0.03) +
  geom_sf(data=cosub, size=0.2, color='white', fill=NA)+  
  ggsflabel::geom_sf_text_repel(data=label, color='white',fontface='bold',point.size=NA, mapping=aes(label=NAME), size=3)+  
  geom_sf(data=county, size=0.5, color='white', fill=NA)+ 
  scale_fill_viridis_c(name='Average Commute\nTime (minutes) ')+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_sf(xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4])) +
  labs(
    title=title,
    tag='Andy Arthur, 12/27/2021',
    caption='2019 ACS5 Subject Table S0801_C01_046'
  ) +
  theme_void() + # dark theme
  theme(
    text= element_text(family='Overpass',size=14, color="black"),
    plot.title=element_text(hjust=0.5, face='bold',size=36),
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

ggsave(paste('Desktop/',title,'.svg', sep=''), width=1920, height=1600, units='px', dpi=150, device = grDevices::svg )
ggsave(paste('Desktop/',title,'.jpg', sep=''), width=1920, height=1600, units='px', dpi=150 )
