library(vroom)
library(tidyverse)
library(sf)
library(tigris)
library(ggtext)
library(terra)
library(units)
rm(list=ls())
gnis <- vroom('/media/hd2/draft.redistricting/NationalGNIS.zip')

gnis %>% filter(FEATURE_NAME == 'Hell') %>%
  st_as_sf(coords=c('PRIM_LONG_DEC', 'PRIM_LAT_DEC'), crs=4326,  agr = "constant") %>% st_transform(5070) -> hell


st <- states(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('AK','HI','PR')) %>% st_transform(5070)
cty <- counties(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('AK','HI','PR')) %>% st_transform(5070)

rr <- rast(st, res=10000, crs=crs(hell))
m <- terra::distance(rr, vect(hell))

m <- m * set_units(1,'m') %>% set_units('mi') %>% drop_units()

cty <- cbind(cty, Min=exactextractr::exact_extract(m, cty, fun='median'))

ggplot(cty) + geom_sf(aes(fill=Min), size=0.1) + 
  scale_fill_binned(low = 'red', high='white', breaks=seq(0,1500,200), name='miles', labels=scales::comma_format()) +
  geom_sf(data=hell, color='white') + geom_sf(data=st, fill=NA) +
  ggsflabel::geom_sf_label_repel(data=hell, aes(label=str_c(FEATURE_NAME, ', ', STATE_ALPHA)), family='Roboto Condensed', size=3.2, label.r=0) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('Distance to <span style="color: red; font-size: 46pt">Hell</span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br /><em>USGS GNIS, terra::distance, exactexactr'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, size=28, face='bold'),
    plot.subtitle=ggtext::element_textbox_simple(minheight = 0, height=0),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0.03),
    strip.text = element_text(face='bold'),
    legend.key.height = unit(0.3,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(0.8,1.03),
    legend.key = element_rect(color='white', size=2)
  ) +
  guides(fill = guide_legend(byrow = F))

fn <- str_c('distance-to-hell')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))

