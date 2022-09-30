# load libraries
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)

rm(list=ls())

vars <- load_variables(2020, 'acs5', cache=T)

acs <- get_acs("congressional district", survey='acs5', var='B01001_002', summary_var = 'B01001_001', cache_table = T,
               geometry = T,
               year = 2020, output = "wide")

acs %>%
  mutate(male = B01001_002E/summary_est*100
  ) %>%
  select(GEOID, NAME, male) -> male

male <- male %>% shift_geometry() %>% rmapshaper::ms_simplify(.5)

st <- states(cb=T, resolution = '20m') %>% shift_geometry() 

ggplot(male) + 
  geom_sf(aes(fill=male), size=0.1, color='black') +
  geom_sf(data=st, fill=NA, size=0.6, color='black') +
  scale_fill_gradient2(midpoint = 50, low='red', mid = 'white', high='blue', na.value='gray',name='%\nMale') +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt; color: blue">Gender</span> ',
                     '<span style="font-size: 26pt">By Congressional District</span> '),
           y = "",
       x = "",
       tag=paste('2016-2020 American Community Survey, Table B101002<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.75,0.03),
    legend.position = c(0.8,1.04),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
  ) 

fn <- str_c('gender-cd')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))



