library(tidyverse)
library(sf)
rm(list=ls())


ti <- read_sf('~/Documents/GIS.Data/dot.topoindex/nys_quadindex.shp') %>%
#  st_set_crs(4326) %>%
  st_transform(26918) 

lp <- read_sf('~/Desktop/kml_18562.kmz') %>% st_transform(26918) %>% st_make_valid()

ti %>% st_filter(lp) %>%
  write_sf('/tmp/exportmaps.gpkg')
