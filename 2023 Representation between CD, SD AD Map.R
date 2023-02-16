library(tidyverse)
library(tidycensus)
library(sf)
library(patchwork)

rm(list=ls())

# get district lines from nys gis
cd <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Congressional_Districts/FeatureServer/0')
sd <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Senate_Districts/FeatureServer/0')
ad <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Assembly_Districts/FeatureServer/0')


admap <- 
  ad %>% rmapshaper::ms_simplify() %>%
    ggplot() + geom_sf(aes(fill=PARTY), color='white', linewidth=0) + 
    scale_fill_manual(values=c('blue','red')) +
    theme_void() +
    labs(title = 'NY Assembly') +
    theme(
      legend.position = 'None',
      plot.title = element_text(hjust = 0.5, family = 'Roboto', size = 25)
    )

sdmap <- 
  sd %>% rmapshaper::ms_simplify() %>%
  ggplot() + geom_sf(aes(fill=PARTY), color='white', linewidth=0) + 
  scale_fill_manual(values=c('blue','red')) +
  theme_void() +
  labs(title = 'NY Senate') +
  theme(
    legend.position = 'None',
    plot.title = element_text(hjust = 0.5, family = 'Roboto', size = 25)
  )

cdmap <- 
  cd %>% rmapshaper::ms_simplify() %>%
  ggplot() + geom_sf(aes(fill=PARTY), color='white', linewidth=0) + 
  scale_fill_manual(values=c('blue','red')) +
  theme_void() +
  labs(title = 'NY Congressional') +
  theme(
    legend.position = 'None',
    plot.title = element_text(hjust = 0.5, family = 'Roboto', size = 25)
  )

 (cdmap + sdmap + admap) +
   plot_annotation(
     title = '2023 Representation',
     theme = theme(plot.title = element_text(family = 'Roboto Condensed', hjust = 0.5, face = 'bold', size = 40)),
   ) 
 
 fn <- str_c('2023-representation')
 ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=635, units='px', dpi=120,  device = grDevices::jpeg)
 ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=635, units='px', dpi=120, device = grDevices::svg)
 system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))

 
library(gt)
sd %>% st_drop_geometry() %>% count(PARTY) %>% 
   transmute(
     Party = PARTY,
     Number = n,
     Percent = round(n/sum(n)*100,1)) %>%
    gt()  %>%
  opt_stylize(color = 'gray') 


cd %>% st_intersection(sd) %>% st_intersection(ad) -> di

nyco <- tigris::counties('ny', resolution='20m') %>% rmapshaper::ms_simplify() 

library(ggtext)

di <- cd %>% st_intersection(sd) %>% st_intersection(ad)

di %>%
  st_make_valid() %>%
  st_buffer(0) %>%
  filter(st_area(.)>units::set_units(100,'m^2')) %>%
  mutate(
    trifecta = 'Split',
    trifecta = ifelse(PARTY == 'Democratic' & PARTY.1 == 'Democratic' & PARTY.2 == 'Democratic', 'Democratic', trifecta),
    trifecta = ifelse(PARTY == 'Republican' & PARTY.1 == 'Republican' & PARTY.2 == 'Republican', 'Republican', trifecta)
) %>% 
  group_by(trifecta) %>%
  summarise() %>%
  rmapshaper::ms_simplify() -> tf
  

ggplot(tf) + geom_sf(aes(fill=trifecta), linewidth=0) + 
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.8) +
  scale_fill_manual(values=c('blue','red','purple')) +
  coord_sf(expand=F) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 55pt; color: navy">NYS Trifectas</span><br /><br/><em>Areas where the Congressional, State Senate and Assembly members are of the same party.'),
       y = "",
       x = "",
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: NYS GIS, ArcMap Server.' ),
       fill = "") +
  theme(
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(2.3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25,, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 

fn <- str_c('trifectas')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

tf %>% 
  st_make_valid() %>%
  st_buffer(0) %>%
  mutate( percent = prop.table(st_area(.))*100)

ad %>% 
  mutate(per = st_area(.)/sum(st_area(.))*100) %>%
  st_drop_geometry() %>%
  group_by(PARTY) %>%
  summarise(percent = sum(per))


sd %>% 
  mutate(per = st_area(.)/sum(st_area(.))*100) %>%
  st_drop_geometry() %>%
  group_by(PARTY) %>%
  summarise(percent = sum(per))

cd %>% 
  mutate(per = st_area(.)/sum(st_area(.))*100) %>%
  st_drop_geometry() %>%
  group_by(PARTY) %>%
  summarise(percent = sum(per))
