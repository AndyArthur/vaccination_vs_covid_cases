library(tidyverse)
library(tigris)
library(sf)
library(units)
rm(list=ls())

usco <- counties(cb=T,resolution = '20m') %>% filter(!STUSPS %in% c('HI','AK','PR')) %>% rmapshaper::ms_simplify() %>% st_transform(5070)

usst <- states(resolution = '20m') %>% filter(GEOID < 57 & !STUSPS %in% c('HI','AK','PR')) %>% rmapshaper::ms_simplify()

trainspill <- st_as_sf(st_sfc(st_point(c(-80.5227, 40.8360))), crs=4326) %>% 
  st_transform(5070)

usco %>% 
  mutate(dist = st_distance(., trainspill) %>% 
           set_units('mi') %>% drop_units()) %>%
  ggplot() + 
  geom_sf(aes(fill=dist), linewidth=0.1, color='white') +
  geom_sf(data=usst, fill=NA, linewidth=0.6, color='white') +
  scale_fill_viridis_b(breaks=seq(0,400,50), option='B', direction=-1, name='Miles from Derailment') +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('Distance from the<br /><span style="font-size: 40pt">2023 Ohio Train Derailment</span>'),
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%d/%y"),
                 '<br />Source: Based on the distance from 40.8360, -80.5227 the reported location of the derailment on Wikipedia.'),
  )  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=20, width=0.7, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555',  halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = 'bottom',
    legend.position = c(1,1.04),
    legend.justification = 'right',
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.title = element_text(face='italic', size=12)
  ) +
  guides(fill=guide_colorsteps(ticks = F, title.position='bottom', title.hjust=0.5))

fn <- str_c('2023-train-derail')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1350, units='px', dpi=130, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1350, units='px', dpi=130, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))


