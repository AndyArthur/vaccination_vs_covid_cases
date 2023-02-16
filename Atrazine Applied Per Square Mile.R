library(tidyverse)
library(tigris)
library(sf)
library(units)

rm(list=ls())

ep <- read_tsv('~/Downloads/EPest_county_estimates_2019.txt')

ep <- ep %>% mutate(GEOID = str_c(STATE_FIPS_CODE, COUNTY_FIPS_CODE))

usco <- counties(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('GU', 'AK','HI','PR')) %>%
  rmapshaper::ms_simplify()

usst <- states(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('GU', 'AK','HI','PR')) 

usco %>% left_join(ep, join_by(GEOID), multiple='all') %>%
  filter(COMPOUND == 'ATRAZINE') %>%
  mutate(ppmi = (set_units(EPEST_HIGH_KG, 'kg') %>% set_units('lb') %>% drop_units())/(st_area(.) %>% set_units('mi^2') %>% drop_units())) %>%
  ggplot() + 
  geom_sf(data=usco, aes(fill=0),color='white', linewidth=0.05) +
  geom_sf(aes(fill=ppmi), color='white', linewidth=0.05) +
  geom_sf(data=usst, fill=NA, color='white', linewidth=0.4) +
  scale_fill_viridis_b(label=scales::label_comma(), option = 'G', breaks=c(25,50,100,200,300,400,500),
                       name = 'Pounds of Atrazine Applied Per Square Mile') +
  coord_sf(crs=5070, expand=F) +
  theme_void() +
  labs(title = str_c('<span style="color: navy; font-size: 36pt">Atarazine</span> Pesticide Applied in 2019'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br /><em>National Water-Quality Assessment (NAWQA) Project: Pesticide National Synthesis Project (High - Imputed Model), water.usgs.gov/nawqa/pnsp/usage/maps/county-level/'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "gray96", color="gray96"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0.03),
    strip.text = element_text(face='bold'),
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(1,1.02),
    legend.title = element_text(size=11, face = 'italic'),
    legend.justification = 'right'
  ) +
  guides(fill=guide_colorsteps(ticks = F, title.position='bottom', title.hjust=0.5))


fn <- str_c('atarzine')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))




