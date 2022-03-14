library(tigris)
library(tidyverse)
library(sf)

ri <- states() %>% filter(STUSPS == 'RI') %>% st_drop_geometry()

counties <- counties(cb=T, resolution='20m')

rico <- counties %>% filter(ALAND > ri$ALAND) %>% 
  shift_geometry()

states <- states(cb=T, resolution='20m') %>% filter(GEOID < 60) %>% shift_geometry()

ggplot() +
  geom_sf(data=rico, fill='tan', size=0.1, color='black') +
  geom_sf(data=states, fill=NA, color='black')  +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = "Counties with a Landmass Larger then Rhode Island",
       y = "",
       x = "",
       caption='Census TIGER/Line',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    plot.title=element_text(hjust=0.5, family='Overpass', face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(family='Overpass',size=10,hjust=0, color='#555555'),
    plot.caption=element_text(family='Overpass', size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'bottom',
  ) 

ggsave(paste('Desktop/Larger then Rhode Island.svg',sep=''), width=1920, height=1200, units='px', dpi=150, device = grDevices::svg)
ggsave(paste('Desktop/Larger then Rhode Island.jpg',sep=''), width=1920, height=1200, units='px', dpi=150)


 
