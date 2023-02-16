library(tidyverse)
library(terra)
library(stars)
library(ggtext)

rm(list=ls())


norm <- rast('/home/andy/Desktop/jan/PRISM_tmean_30yr_normal_4kmM4_01_bil.bil')
jan23 <- rast('/home/andy/Desktop/jan/PRISM_tmean_provisional_4kmM3_202301_bil.bil')


dep <- ((jan23  * 9/5) + 32) -  ((norm  * 9/5) + 32)

st <- 'New York'
ny <- tigris::states(cb=T) %>% filter(NAME==st) %>% st_transform(3857)
ny2 <- terra::project(vect(ny), crs(dep))

nyc <- tigris::counties(st, cb=T)
nycs <- tigris::county_subdivisions(st, cb=T)

s <- crop(dep, ny2, mask=T)
st_contour(st_as_stars(s), breaks = seq(-20,20), contour_lines = F) %>%
  st_as_sf() %>% st_transform(3857) %>% st_intersection(ny) -> contour

ggplot(contour) + geom_sf(aes(fill=Min), linewidth=0.1) +
  geom_sf(data=ny, fill=NA, linewidth=1) +
  geom_sf(data=nyc, fill=NA, linewidth=0.5) +
  scale_fill_viridis_b(option = 'C', name='deg. F\nabove average') +
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: navy; font-size: 50pt">January 2023 Temperature Deviation</span> <em>from 30-year Normals'),
       y = "",
       x = "",
       tag = paste('',
                   'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br />Source: PRISM Mean Temperature, Minus Climate 30 Year Normals.'),
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
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
  ) 

fn <- str_c('januarydeviation')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

