library(tigris)
library(tidyverse)
library(classInt)
library(tidycensus)
library(sf)
library(wacolors)

rm(list=ls())

vars <- load_variables(2020, 'acs5', cache=T)

acs <- get_acs("county subdivision",
               state='ny',
               survey='acs5',
               variables = c(
                 'Natural Gas'='B25040_002',
                 'Propane'='B25040_003',
                 'Electricity'='B25040_004',
                 'Oil'='B25040_005',
                 'Coal'='B25040_006',
                 'Wood'='B25040_007',
                 'Solar'='B25040_008',
                 'None'='B25040_009'
                 ), 
               summary_var = 'B25040_001',
               cache_table = T,
               geometry = T,
               year = 2021
               )

acs <- rmapshaper::ms_simplify(acs)

acs %>% filter(!variable %in% c('Solar','None')) %>%
ggplot() + geom_sf(aes(fill=estimate/summary_est), linewidth=0) + 
  scale_fill_viridis_b(option='B', breaks=seq(0,1,.1), labels=scales::percent_format()) +
  facet_wrap(~variable,  strip.position="left") + theme_void() +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 32pt; color: darkred">Primary Home Heating Method</span> ',
                     '<span style="font-size: 14pt">in New York State</span> '),
       y = "",
       x = "",
       tag=paste('2021 American Community Survey 5 yr, Table B25040 - ',
                 'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', halign = 1, margin=margin(20,0,0,0)),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0),
    strip.text = element_text(size='14', face='bold', margin=margin(0,0,10,0)),
    legend.position = c(0.8,1.04),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(2,'cm'),
  ) 

fn <- str_c('heating-method')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1000, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1000, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

acs %>% st_drop_geometry() %>% group_by(variable) %>% summarise(total = sum(estimate)/sum(summary_est)*100) %>% arrange(-total)


