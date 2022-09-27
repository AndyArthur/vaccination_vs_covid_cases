library(tidyverse)
library(terra)
library(sf)
library(units)
rm(list=ls())

evs <-'https://services8.arcgis.com/MVX6tbvWftyS3KBR/ArcGIS/rest/services/Albany_County_Polling_Places/FeatureServer/0'
evs <- arcpullr::get_spatial_layer(evs)
evs <- st_transform(evs, 26918)

uri <- 'https://services8.arcgis.com/MVX6tbvWftyS3KBR/ArcGIS/rest/services/Albany_Couty_Election_Districts/FeatureServer/0'
eds <- arcpullr::get_spatial_layer(uri)

ct <- counties(state='ny', cb=T) %>% filter(NAME=='Albany') %>% st_transform(26918)

rr <- rast(ct, res=100, crs=crs(evs))
m <- terra::distance(rr, vect(evs))

m <- m * set_units(1,'m') %>% set_units('mi') %>% drop_units()

dc <- cbind(eds, Min=exactextractr::exact_extract(m, eds, fun='median'))
dc$lab <- cut(dc$Min, breaks = seq(0,20,1), labels=str_c(seq(0,19,1),' mi'))
  

cts <- county_subdivisions('ny', 'albany')

ggplot(dc) + geom_sf(aes(fill=lab), size=0.1) +
  geom_sf(data=cts, fill=NA, size=0.3) +
  geom_sf(data=evs, color='white', size=1) +
  geom_sf(data=ct, size=1,fill=NA) +
  wacolors::scale_fill_wa_d(palette = 'forest_fire', reverse = 1) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('Median Distance to an <br /><span style="color: #FFD200; font-size: 36pt">Albany County<br />Early Voting Site</span>'),
       subtitle = '<br />Nearly 3 in 4 Albany County residents live within 3 miles<br /> of an Early Voting site.',
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br /><em>Albany County REST/Services, terra::distance, exactexactr'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, size=28, face='bold'),
    plot.subtitle=ggtext::element_textbox_simple(minheight = 0, height=0),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.8,0.03),
    strip.text = element_text(face='bold'),
    legend.key.height = unit(0.3,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(0.86,1.1),
    legend.key = element_rect(color='white', size=2)
  ) +
  guides(fill = guide_legend(byrow = F))

fn <- str_c('median-dist-ev')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))


