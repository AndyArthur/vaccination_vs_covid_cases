library(sf)
library(tigris)
library(tidyverse)
library(terra)
rm(list=ls())

nlcd <- terra::rast('~/Documents/GIS.Data/agriculture/nlcd2019_ny.tif')

cp <- read_sf('~/Documents/GIS.Data/dot.parklands/AdirondackCatskill.shp') %>%
  filter(Name == 'Catskill Park') %>% st_transform(5070)

nlcd <- nlcd %>% crop(as_Spatial(cp))
nlcd <- mask(nlcd, cp) %>% project('epsg: 3857')
nlcd10 <- nlcd %>% aggregate(fact=3, fun='median')
nlcd.poly <- as.polygons(nlcd10)
nlcd.poly <- nlcd.poly %>% st_as_sf() %>% st_transform(3857)

nlcd.poly <-nlcd.poly %>% 
  left_join(FedData::nlcd_colors(), join_by(`NLCD Land Cover Class` == ID)) %>% 
  mutate(Percent = (st_area(.) / sum(st_area(.))) %>% units::drop_units(), 
         Label = str_c(Class, ', ', scales::percent(Percent, accuracy=0.1)))

cat.cos <- county_subdivisions('ny', cb=T) %>% 
  st_transform(5070) %>%
  st_intersection(cp)

cat.cty <- counties('ny', cb=T) %>% 
  st_transform(5070) %>%
  st_intersection(cp)

ggplot() + geom_sf(data=nlcd.poly, aes(fill=`Color`), linewidth=0) +
  geom_sf(data=cat.cos, fill=NA) +
  geom_sf(data=cat.cty, fill=NA, linewidth=1) +
  geom_sf(data=cp, fill=NA, linewidth=1, color='darkblue') +
  scale_fill_identity(breaks = nlcd.poly$Color, labels = nlcd.poly$Label, guide = "legend", name='') +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 55pt">Land Cover in the <br /><b style="font-size: 70pt; color: darkblue">Catskill Park</b></span><br />',
                     '2019 National Land Cover Dataset<br />Andy Arthur, 2/4/23'),
      )  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox(hjust=0, face='bold', maxwidth = 0.4, height=0),
    plot.background = element_rect(fill = "mintcream", color="mintcream"),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    legend.justification = 'center',
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(0.6,'cm')
  ) 

fn <- str_c('cp-landcover')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

