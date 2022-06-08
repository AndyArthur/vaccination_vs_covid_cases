library(suncalc)
library(timezone)
library(tidyverse)
library(sf)
library(tigris)
library(purr)
library(lubridate)
library(ggtext)

usco <- county_subdivisions('ny', cb=T) 
usco$tz <- usco %>% st_centroid() %>% st_coordinates() %>% find_tz()
usco <- cbind(usco %>% st_centroid() %>% st_coordinates(), usco)

usco <- cbind(usco, getSunlightTimes(data=data.frame(date=Sys.Date(), lat=usco$Y, lon=usco$X, tz=usco$tz)))
usco <- usco %>% st_set_geometry('geometry')

ggplot(usco) + 
  geom_sf(aes(fill=sunset), size=0) +
  geom_sf(data=counties('ny', cb=T), fill=NA) +
  coord_sf(crs=3857, expand=F) + 
  scale_fill_datetime(low='darkblue', high='orange', date_labels='%-I:%M%p', timezone=first(usco$tz)) +
  theme_void() +
  labs(title = str_c('<span style="color: purple">Sunset Time</span> County Population'),
       y = "",
       x = "",
       caption='2020 US Census',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=30, margin=unit(c(5,0,3,0),'pt'), lineheight = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, face='italic', margin=unit(c(0,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'top',
  ) 

fn <- str_c('texas-pop')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))




