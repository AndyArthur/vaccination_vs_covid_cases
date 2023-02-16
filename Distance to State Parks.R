library(tidyverse)
library(tigris)
library(sf)
library(terra)
library(MapColoring)
library(units)

rm(list=ls())
parks <- read_sf('~/Documents/GIS.Data/oprhp.trails/oprhp16.shp') %>%
  filter(Name != 'inholding' &
           Category == 'State Park') %>%
  st_transform(26918)

ny <- states(cb=T) %>% filter(NAME == 'New York') %>% st_transform(26918)

rr <- rast(ny, res=1000, crs=crs(parks))
m <- terra::distance(rr, vect(parks))

k <- m * set_units(1,'m') %>% set_units('mi') %>% drop_units()
  
ct <- stars::st_contour(stars::st_as_stars(k), breaks=seq(0,70,5), contour_lines = F)

ny <- states(cb=T) %>% filter(STUSPS == 'NY') %>% st_transform(26918)

ct <- ct %>% st_intersection(ny)

cs <- counties('ny',cb=T, resolution = '500k') %>% rmapshaper::ms_simplify()

library(ggtext)
ggplot(ct) + geom_sf(aes(fill=factor(Min)),linewidth=0.1) + scale_fill_viridis_d(name='Miles') +
  geom_sf(data=parks, fill='darkgreen', color='darkgreen', linewidth=0.4) +
  geom_sf(data=cs, fill=NA, color='white',  linewidth=0.4) +
  geom_sf(data=ny, fill=NA, color='black',  linewidth=0.9) +
  
  coord_sf(expand=F, crs=3857) + 
    theme_void() +
  labs(title = str_c('Distance to<br /><span style="color: darkgreen; font-size: 85pt;font-weight: bold">State<br />Parks</span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: OPRHP, terra::distance, 5 mile contours'),
       fill = "")  +
  theme(
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, width=0.5, face='bold',size=45, margin=unit(c(15,0,15,0),'pt'), maxheight=0),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
  ) 

fn <- str_c('stat-park-dist')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


