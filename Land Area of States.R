library(tigris)
library(tidyverse)
library(sf)

states <- states(cb=T, resolution='20m') %>% filter(GEOID < 60) %>% shift_geometry()
states$ALANDmi <- NISTunits::NISTsqrMeterTOsqrMile(states$ALAND)*100

ggplot(states) + geom_sf(aes(fill=ALAND),size=0.3, color='black') +
  ggsflabel::geom_sf_label_repel(aes(label=scales::comma(ALANDmi,scale = 1/1000, accuracy = 1)), 
                                 point.size = NA, size=3, label.padding = unit(0.15, "lines") ) +
  scale_fill_distiller(palette = 'Greens', direction = 1
                       ) +
  coord_sf(expand=F) +
  labs(title = "Land Area of States",
       y = "",
       x = "",
       caption='Thousand Square Miles - Census TIGER/Line',
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
    legend.position = 'none'
  ) 

type <- 'Land Area of States'
ggsave(paste('/tmp/',type,'.jpg',sep=''), width=1920, height=1400, units='px', dpi=150)
ggsave(paste('/tmp/',type,'.svg',sep=''), width=1920, height=1400, units='px', dpi=150, device = grDevices::svg)

                                 