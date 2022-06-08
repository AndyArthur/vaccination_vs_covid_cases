library(sf)
library(tidyverse)
library(tigris)

er <- read_sf('/tmp/id2020.gpkg') %>% st_transform(3857) %>% st_centroid()

cos <- counties('id', cb=T) %>% st_transform(3857)

er %>% 
  st_join(cos) %>% 
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(across(5:11, sum)) %>%
  inner_join(paco, ., by=c('GEOID')) %>%
  write_sf('/tmp/ido.gpkg')
