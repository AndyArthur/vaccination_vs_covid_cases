library(ggtext)
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)

tr <- read_csv('~/Downloads/Active_Tobacco_Retailer_Map.csv')

tr %>% 
  mutate(lat = str_match(LOCATION, '\\((.*?), (.*?)\\)')[,2],
         lng = str_match(LOCATION, '\\((.*?), (.*?)\\)')[,3]
         ) %>% 
  drop_na() %>%
  st_as_sf(coords=c('lng', 'lat'), crs=4326) %>% st_transform(3857) -> tr

nyco <- counties('ny') %>% st_transform(3857)
nycos <- get_decennial('county subdivision', state='ny', variables = "P001001", geometry = T) %>% st_transform(3857)

tr %>% st_intersection(nycos) %>% st_drop_geometry() -> trn

trn %>% group_by(GEOID) %>%
  summarise(ct = n()) -> trc

nycos %>% left_join(trc, join_by(GEOID)) %>%
  mutate(trpc = ct/value*10000) %>%
  ggplot() + geom_sf(aes(fill=trpc), color='white',linewidth=0.01) +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) +
  scale_fill_viridis_b(n.breaks=8, option='F', na.value='black') +
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: darkred; font-size: 60pt">Tobacco Retailers Per 10,000 Residents</span>'),
       y = "",
       x = "",
       tag = paste('<span style="color: gray20; font-size: 30pt">In New York State there are on average 8.4 tobacco retailers per 10,000 residents.</span><br /><span style="font-size:36pt">&nbsp;</span><br />',
                   'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br />Source: NYS Active Tobacco Retailer Map. health.data.ny.gov/Health/Active-Tobacco-Retailer-Map/88k2-euek'),
       fill = "")  +
  theme(
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=46, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555', width=0.7, valign=0, vjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(2.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
  ) 

fn <- str_c('tobacco-retailers-cosub')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


nycos %>% left_join(trc, join_by(GEOID)) %>%
  mutate(trpc = ct/value*10000) %>%
  ggplot() + geom_sf(aes(fill=trpc), color='white',linewidth=0.01) +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) +
  scale_fill_viridis_b(n.breaks=8, option='F', na.value='black') +
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: darkred; font-size: 60pt">Tobacco Retailers Per 10,000 Residents</span>'),
       y = "",
       x = "",
       tag = paste('<span style="color: gray20; font-size: 30pt">In New York State there are on average 8.4 tobacco retailers per 10,000 residents.</span><br /><span style="font-size:36pt">&nbsp;</span><br />',
                   'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br />Source: NYS Active Tobacco Retailer Map. health.data.ny.gov/Health/Active-Tobacco-Retailer-Map/88k2-euek'),
       fill = "")  +
  theme(
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=46, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555', width=0.7, valign=0, vjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(2.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
  ) 

fn <- str_c('tobacco-retailers-cosub')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


