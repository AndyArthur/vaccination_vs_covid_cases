library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
rm(list=ls())
cds <- read_sf('~/Downloads/tlgdb_rd22_a_us_legislative.gdb.zip')

cds <- cds %>% rmapshaper::ms_simplify(0.01) %>% filter(GEOID < 5800)

usm <- states(resolution = '20m') %>%  filter(GEOID < 58) %>% st_transform(3857) %>% st_union() %>% st_buffer(20000)

ocean <- rnaturalearth::ne_load(type='ocean', category = 'physical', scale='medium', destdir = '~/.cache/RStudio/') %>% 
  st_as_sf() %>% st_transform(3857) %>% st_intersection(usm) %>% st_union()
lakes <- rnaturalearth::ne_load(type='lakes', category = 'physical', scale='medium', destdir = '~/.cache/RStudio/') %>% 
  st_as_sf() %>%  filter(min_zoom == 1) %>% st_transform(3857)  %>% st_intersection(usm) %>% st_union()

cds <- cds %>% st_transform(3857) %>% st_difference(ocean) %>% st_difference(lakes) %>% shift_geometry()

usst <- states(resolution = '20m', cb=T) %>% filter(GEOID < 5800) %>% shift_geometry()

dp02 <- read_csv('~/Downloads/DP02_1yr_500.csv')

dp02 <- dp02 %>% mutate(GEOID = substr(GEOID, 10, 14)) %>% filter(PROFLN == 18)

cds %>% filter(GEOID < 5800) %>%
  left_join(dp02, join_by(GEOID)) %>%
  st_transform(5070) %>%
  mutate(PRF_ESTIMATE = parse_number(PRF_ESTIMATE), persqmi = PRF_ESTIMATE/st_area(.) %>% units::set_units('mi^2') %>% units::drop_units() ) %>%
  drop_na(PRF_ESTIMATE) %>%
  ggplot() + geom_sf(aes(fill=persqmi), color='gray80', linewidth=0.05) + 
  scale_fill_viridis_b(labels=scales::label_comma(), n.breaks=8, trans='log10', name='Residents/sq mi') +
  geom_sf(data=usst, fill=NA, linewidth=0.4, color='gray80',) + 
  theme_void() +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt; color: darkblue">Population Density</span><br />',
                     '<span style="font-size: 26pt">2023 Congressional Districts</span>'),
       y = "",
       x = "",
       tag=paste('2021 American Community Survey (ACS) 1-year estimates for 118th Congress.<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m /%-d/%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=12, width=0.5, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.03),
    legend.position = c(0.8,1.04),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
  ) +
  guides(fill = guide_colourbar(title.position = "top"))

fn <- str_c('congress-pop')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=130)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

cdo <- read_sf('~/Downloads/tlgdb_rd22_a_us_legislative.gdb.zip') %>% filter(GEOID < 5800) 

cdo %>% left_join(dp02, join_by(GEOID)) %>%
  st_transform(5070) %>%
  mutate(PRF_ESTIMATE = parse_number(PRF_ESTIMATE), persqmi = PRF_ESTIMATE/st_area(.) %>% units::set_units('mi^2') %>% units::drop_units() ) %>%
  arrange(-persqmi) %>% mutate(Rank = row_number()) %>% 
  slice_max(persqmi, n=50) %>% write_sf('/tmp/mostpopulated.gpkg')
