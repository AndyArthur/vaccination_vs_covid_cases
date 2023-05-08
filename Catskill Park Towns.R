library(tidyverse)
library(sf)
library(tigris)

cp <- read_sf('~/Documents/GIS.Data/dot.parklands/AdirondackCatskill.shp') %>% 
  filter(Name== 'Catskill Park') %>% st_transform(5070) %>%
  st_buffer(units::set_units(-0.25,'mi'))


county_subdivisions('ny') %>% st_transform(5070) %>% 
  st_filter(cp, join='st_overlaps') %>%
  write_sf('/tmp/catskillparktowns.shp')
