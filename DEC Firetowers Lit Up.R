library(arcpullr)
library(tidyverse)
da <- get_spatial_layer('https://services6.arcgis.com/DZHaqZm9cxOD4CWM/ArcGIS/rest/services/NYS_State_Land_Assets/FeatureServer/0')

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
  add_row(NAME='Cathedral Rock',geoms=st_sfc(st_point(c(-74.91545, 44.15322)))) %>%
  add_row(NAME='Cornell Hill',geoms=st_sfc(st_point(c(-73.72195, 43.141365)))) %>%
  add_row(NAME='Sweede',geoms=st_sfc(st_point(c(-73.582871, 43.734901)))) %>%
  add_row(NAME='Mt Morris',geoms=st_sfc(st_point(c(-74.475556, 44.159722)))) %>%
  add_row(NAME='Page Pond',geoms=st_sfc(st_point(c(-75.495,42.147778)))) %>%
  add_row(NAME='Utsayatha',geoms=st_sfc(st_point(c(-74.5895, 42.399075)))) %>%
  add_row(NAME='Stissing',geoms=st_sfc(st_point(c(-73.6928, 41.9566)))) %>%
  add_row(NAME='Spectulator',geoms=st_sfc(st_point(c(-74.359506, 43.497398))))

nyco <- tigris::counties('ny',cb=T)

ggplot() + 
  geom_sf(data=nyco, fill='beige') +
  geom_sf(data=landss, fill='darkgreen',alpha=0.3,size=0) +
  geom_sf(data=firetower) +
  ggsflabel::geom_sf_label_repel(data=firetower, aes(label=str_to_title(NAME)), min.segment.length = 0, size=2.5, family='Noto Sans') +
  theme_void() +
  labs(
    title='NYS Firetowers (32) Participating in <br /><br /><span style="color: orange; font-size: 32pt">Light Up the Night 2022</span><br /><br />September 3, 2022 - 9 PM',
    tag = 'Andy Arthur, 8/31/22. Details: nysffla.org/light.html') +
  coord_sf(expand=F) +
  theme(
    text= element_text(family='Noto Sans',size=12, color="black"),
    plot.title=ggtext::element_textbox(hjust=0, face='bold',size=24, maxheight=0, family='Noto Sans'),
    plot.background = element_rect(fill = "White", size=0),
    panel.background = element_rect(fill = "White", size=0),
    plot.tag=element_text(size=10,hjust=0, color='black'),
    plot.caption=element_text(size=10, color='black'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(3,'cm'),
    legend.position = 'bottom',
  )

fn <- str_c('firetower-lit')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

write_csv(firetower, '/tmp/firetower.csv')
