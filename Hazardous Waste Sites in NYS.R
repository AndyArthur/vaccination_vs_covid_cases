library(tidyverse)
library(sf)
library(tidycensus)

rm(list = ls())

rem <- arcpullr::get_spatial_layer('https://services6.arcgis.com/DZHaqZm9cxOD4CWM/arcgis/rest/services/Remediation_Boundaries/FeatureServer/0/')
rbf <- rem %>% st_transform(26918) %>% st_buffer(units::set_units(1.8,'mi'))

op <-  get_decennial(geography = "block", variables = "P1_001N", 
                     year = 2020, cache=T, geometry = T, state='ny') %>% st_transform(26918)

op %>% st_intersection(rbf) -> rop

rop %>% st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(value = first(value)) %>%
  ungroup() %>%
   group_by(substr(GEOID,1,5)) %>% summarise(value = sum(value)) -> dz

op %>% st_drop_geometry() %>%
  group_by(substr(GEOID,1,5)) %>% summarise(value = sum(value)) -> pop

nyco <- tigris::counties('ny',cb=T)
library(ggtext)

pl <- viridisLite::viridis(n=8)
pop %>% 
  inner_join(dz, join_by(`substr(GEOID, 1, 5)`)) %>%
  transmute(
    GEOID = `substr(GEOID, 1, 5)`,
    Percent = value.y/value.x
  ) %>%
  left_join(nyco, .) %>%
  ggplot() + geom_sf(aes(fill=Percent), linewidth=0.5, color='white') +
  scale_fill_viridis_b(n.breaks=8, labels=scales::label_percent()) +
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: ',pl[4],'; ">County\'s Population<br />within 1.8 miles of NYS Hazardous Remediation Site</span>'),
       y = "",
       x = "",
       tag = paste('
"Research shows adverse health effects most likely occur <br />within a 1.8 mile boundary around a Superfund site." - EPA<br /><br />',
                   'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br /><br />Source: NYS DEC Remediation Parcels buffered out 1.8 miles;<br /> 2020 US Census Block Level Population'),
       fill = "")  +
  theme(
    text= element_text(family='chivo',size=18),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=40, margin=unit(c(30,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=16,hjust=0, color='#555555', valign=0, vjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(3.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
  ) 

fn <- str_c('hazsite')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

