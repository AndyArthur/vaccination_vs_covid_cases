library(tidyverse)
library(tigris)
library(sf)
rm(list=ls())

library(tidycensus)

hs <- read_csv('https://theunitedstates.io/congress-legislators/legislators-current.csv')

unzip('/home/andy/Downloads/2022 National Congressional Districts for the 118th Congress.zip', exdir='/tmp/')
cd <- read_sf('/tmp/2022 U.S. House of Representatives Districts with Water Clipped to Shoreline.shp') %>% 
  rmapshaper::ms_simplify() %>% 
  shift_geometry() %>%
  st_transform(5070)

cd <- st_make_valid(cd) %>% rmapshaper::ms_simplify()


stfip <- fips_codes %>% select(state, stfips = state_code) %>% unique()

st <- states(cb=T, resolution = '20m') %>% filter(STUSPS != 'PR') %>% shift_geometry()

hs %>% filter(is.na(senate_class)) %>% inner_join(stfip, by=c('state')) %>% 
  mutate(Code = str_c(state, '-', str_pad(replace_na(district,0),2, pad = '0')),
         Code = str_replace(Code,'00','AL')) %>% 
  right_join(cd, by=c('Code')) %>%
  mutate(party = ifelse(is.na(party),'Vacant',party)) %>%
  st_set_geometry('geometry') -> hsm


hsm %>%
  ggplot() + 
  geom_sf(aes(fill=party),linewidth=0.1, color='white') +
  geom_sf(data=st, fill=NA, color='white', linewidth=0.6) +
  scale_fill_manual(values = c('blue','red','yellow')) +
  theme_void() +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt">Party Control of Congress </span>',
                     '<span style="font-size: 26pt">Janaury 2023.</span><br />There are 222 Republicans and 212 Democrats in Congress currently with the seat in Virginia 4th vacant pending a special election.'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%d/%y"),
                 '<br />Source: theunitedstates.io Current Congress List and Daily Kos. drive.google.com/file/d/1PuFNnBELeU1sz5jO56osHJT7u8_Dg5k-/view'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "snow", color="snow"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.03),
    legend.key.height = unit(0.3,'cm'),
    legend.key.width = unit(0.3,'cm'),
    legend.text = element_text(margin = margin(t = 30, unit = "pt")),
  ) 

fn <- str_c('party-congress')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))




