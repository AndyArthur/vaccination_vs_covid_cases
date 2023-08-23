library(arcpullr)
library(tidyverse)
da <- get_spatial_layer('https://services6.arcgis.com/DZHaqZm9cxOD4CWM/ArcGIS/rest/services/NYS_State_Land_Assets/FeatureServer/0') %>% st_transform(4326)

lands <- get_spatial_layer('https://services6.arcgis.com/DZHaqZm9cxOD4CWM/ArcGIS/rest/services/NYS_DEC_Lands/FeatureServer/0')

landss <- rmapshaper::ms_simplify(lands)

firetower <- da %>% filter(ASSET == 'FIRE TOWER' & 
                           !NAME %in% 
                             c("PILLSBURY MOUNTAIN FIRE TOWER",
                               "POKE-O-MOONSHINE FIRE TOWER",
                               "MOUNT NIMHAM FIRE TOWER"  ,
                               "ROOSA GAP FIRETOWER"  ,
                               "SUGAR HILL FIRE TOWER" ,
                               "L&F FIRE TOWER",
                               "LEONARD HILL FIRE TOWER",
                               "FIRE TOWER",
                               "RONDAXE FIRE TOWER",
                               "LOON LAKE MTN. FIRE TOWER",
                               "LYON MOUNTAIN FIRE TOWER"
                           )) %>%
  mutate(NAME = str_replace(NAME, ' FIRE TOWER', ''),
         NAME = str_replace(NAME, ' FIRETOWER', ''),
         NAME = str_replace(NAME, ' Fire Tower', ''),
         NAME = str_replace(NAME, ' Mountain', ''),
          NAME = str_replace(NAME, ' MOUNTAIN', ''),
         NAME = str_replace(NAME, ' CABIN', '')) %>% 
  add_row(NAME='Cathedral Rock',geoms=st_sfc(crs=4326, st_point(c(-74.91545, 44.15322)))) %>%
  add_row(NAME='Cornell Hill',geoms=st_sfc(crs=4326, st_point(c(-73.72195, 43.141365)))) %>%
  add_row(NAME='Sweede',geoms=st_sfc(crs=4326, st_point(c(-73.582871, 43.734901)))) %>%
  add_row(NAME='Mt Morris',geoms=st_sfc(crs=4326, st_point(c(-74.475556, 44.159722)))) %>%
  add_row(NAME='Page Pond',geoms=st_sfc(crs=4326, st_point(c(-75.495,42.147778)))) %>%
  add_row(NAME='Utsayatha',geoms=st_sfc(crs=4326, st_point(c(-74.5895, 42.399075)))) %>%
  add_row(NAME='Stissing',geoms=st_sfc(crs=4326, st_point(c(-73.6928, 41.9566)))) %>%
  add_row(NAME='SpectuChivor',geoms=st_sfc(crs=4326, st_point(c(-74.359506, 43.497398)))) %>%
  add_row(NAME='Ferncliff Forest',geoms=st_sfc(crs=4326, st_point(c(-73.926667, 41.956944)))) 
  
nyco <- tigris::counties('ny',cb=T)

ggplot() + 
  geom_sf(data=nyco, color='#9F8C76', linewidth=2) +
  geom_sf(data=nyco, fill='beige', color='white', linewidth=0.8) +
  geom_sf(data=landss, fill='darkgreen',alpha=0.3,linewidth=0) +
  geom_sf(data=firetower) +
  ggsflabel::geom_sf_label_repel(data=firetower, aes(label=str_to_title(NAME)), min.segment.length = 0, size=2.5, family='Chivo') +
  theme_void() +
  labs(
    title='Firetowers (33) Participating in <br /><br /><span style="color: orange; font-size: 62pt">Light Up the<br />Night 2023</span><br /><br />Saturday, September 2 at 9 PM',
    tag = 'Andy Arthur, 8/22/23. <br /><br />Details: nysffla.org/light.html') +
  coord_sf(expand=F) +
  theme(
    text= element_text(family='Chivo',size=12, color="black"),
    plot.title=ggtext::element_textbox(hjust=0, face='bold',size=24, maxheight=0, maxwidth=20, family='Chivo' ),
    plot.background = element_rect(fill = "White", size=0),
    panel.background = element_rect(fill = "White", size=0),
    plot.tag=ggtext::element_textbox(size=14,hjust=0, color='black'),
    plot.caption=element_text(size=10, color='black'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(3,'cm'),
    legend.position = 'bottom',
  )

fn <- str_c('firetower-lit')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1500, units='px', dpi=150)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1500, units='px', dpi=150, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

library(gt)
firetower %>%
  transmute(
    Tower = str_to_title(NAME),
    Latitude = st_coordinates(geoms)[,2],
    Longtitude = st_coordinates(geoms)[,1]) %>%
  st_drop_geometry() %>%
  arrange(Tower) %>%
  gt() %>%
  gtExtras::gt_theme_538() %>%
  tab_options(table.width = px(500)) %>%
  tab_footnote(html(
    'Andy Arthur, 8/21/23. 
    <div style="float: right">More Info: <a href="https://www.nysffla.org/light.html">nysffla.org/light.html</a>.')) -> output_table

output_table  %>% tab_header(
  title = md("**Firetowers Participating in Light Up the Night 2023**<br /><span style=\"font-size: 70%\">These towers will be lit up on Saturday, September 2nd at 9 PM.")
) -> output_header

output_header

output_header %>% gtsave('/tmp/firetower-table.png', expand=c(0,20,5,10)) 

