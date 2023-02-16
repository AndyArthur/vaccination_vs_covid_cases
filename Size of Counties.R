library(tidyverse)
library(tigris)
library(units)
library(sf)
rm(list=ls())

usco <- counties(cb=T, resolution = '20m') %>% st_transform(5070)
usst <- states(cb=T, resolution = '20m') %>% filter(STUSPS != 'PR') %>% shift_geometry()

clr <- viridis::viridis(6)[4]

usco %>% mutate(area = st_area(.) %>% set_units('mi^2') %>% drop_units()) %>%
  shift_geometry() %>%
  ggplot() + geom_sf(aes(fill=area), linewidth=0.1) + 
  scale_fill_viridis_b(labels=scales::label_comma(), breaks=c(1, 2.5,5,10,20,30)*1e3, name='Square Miles') +
  geom_sf(data=usst, fill=NA, linewidth=0.4) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="color: ',clr,'; font-size: 36pt">Area of US Counties</span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br /><em>US Census, TIGER/LINE'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "#FFFCFF", color="#FFFCFF"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.03),
    strip.text = element_text(face='bold'),
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(1,1.02),
    legend.title = element_text(size=11, face = 'italic'),
    legend.justification = 'right'
  ) +
  guides(fill=guide_colorsteps(ticks = F, title.position='bottom', title.hjust=0.5))

fn <- str_c('area-of-counties')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))



