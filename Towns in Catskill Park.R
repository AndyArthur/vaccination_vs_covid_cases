library(tidyverse)
library(tidycensus)
library(sf)
rm(list=ls())

tb <- get_decennial('block', state='ny', variables = 'P1_001N', year=2020, geometry = T)
tb <- tb %>% st_transform(26918)

cp <- read_sf('~/Documents/GIS.Data/dot.parklands/AdirondackCatskill.shp') %>%
  filter(Name == 'Catskill Park') %>% st_transform(26918)

tb <- tb %>% st_intersection(cp)

towns <- tigris::county_subdivisions('ny', cb=T) %>% 
  st_transform(26918) %>%
  st_intersection(cp)

tb <- tb %>% st_intersection(towns)

tb %>%
  mutate(area = units::set_units(st_area(.),'mi^2')) %>%
  dplyr::group_by(NAMELSADCO, NAMELSAD) %>%
  summarize(pop = sum(value), 
            density = sum(value) / sum(area),
            density = units::drop_units(density),
            area = sum(area),
            area = units::drop_units(area)) %>%
  filter(area > 0.2) %>%
  st_drop_geometry() %>% ungroup() -> tl

library(gt)

tl %>% 
  transmute('County' = NAMELSADCO,
      'Municipality' = NAMELSAD,
      'Population in Park' = pop,
      'Persons, sq/mi' = density,
      'Area in Park' = area
  ) %>%
  .[1:10,] %>%
  gt() %>%
  gt::fmt_integer(3:4) %>%
  gt::fmt_number(5, decimals=1) %>%
  gt::tab_header(
    title = '2020 Population within the Catskill Park') %>%
  gt::gtsave('/tmp/catpark1.png')

tb %>%
  filter(st_area(.) > units::set_units(10,'m^2')) %>%
  st_cast('MULTIPOLYGON') %>%
  rmapshaper::ms_simplify() %>%
  mutate(density = value / units::set_units(st_area(.),'mi^2'),
         density = units::drop_units(density)) %>% 
  ggplot() +
  geom_sf(aes(fill=density), linewidth=0) +
  geom_sf(data=towns, fill=NA, color='white', linewidth=0.6) +
  ggsflabel::geom_sf_text_repel(data=towns %>% filter(st_area(.) > units::set_units(1,'mi^2')), aes(label=NAME), bg.r=0.1, bg.color='black', color='white', size=3, fontstyle='bold', point.size=NA) +
  scale_fill_viridis_b(limits=c(0,2000), breaks=c(0,100,250,500,1000,2000), labels=scales::label_comma()) +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt">Population Density of the Catskill Park</span><br />',
                     'There are 53,614 residents who live within the blue line of the Catskill Park, <br />a population density of 49 residents per square mile.'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%d/%y"),
                 '<br />Source: 2020 US Census'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "mintcream", color="mintcream"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0.03),
    legend.position = c(0., 0.9),
    legend.direction = 'horizontal',
    legend.justification = 'left',
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(3,'cm')
  ) 

fn <- str_c('catskill-park-density')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))



