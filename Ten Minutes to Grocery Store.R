library(tidyverse)
library(sf)
library(mapboxapi)

rm(list=ls())

rf <- read_sf('https://data.ny.gov/api/geospatial/9a8c-vfzj?accessType=DOWNLOAD&method=export&format=GeoJSON')

alb <- county_subdivisions('ny') %>% filter(NAME == 'Albany') #%>%
  st_buffer(set_units(5,'mi')) %>% st_transform(4326)

rf %>% mutate(square_footage = parse_number(square_footage)) %>% 
  filter(grepl('WAL-MART', dba_name) |
           grepl('PRICE C', dba_name) |
           grepl('WALMART', dba_name) |
           grepl('TOPS', dba_name) |
           grepl('HANNAF', dba_name) |
#           grepl('ALDI', dba_name) |
           grepl('WEGMAN', dba_name) |
           grepl('SHOPRIT', dba_name) |
           grepl('SHOP-RIT', dba_name) 
         ) %>%
  st_intersection(alb) -> albstore

alb.dist <-  mb_isochrone(albstore, time=10, profile='walking')

mapview::mapview(alb.dist)

write_sf(alb.dist %>% group_by() %>% summarise(), '/tmp/distance-to-store.gpkg')
write_sf(albstore, '/tmp/albstore.gpkg')


atb <- get_decennial('block', state='ny', county='albany', year=2020, variables = "P1_001N",  geometry = T)


alb.dist <- alb.dist %>% st_transform(26918) %>% group_by() %>% summarise()

atb %>% st_transform(26918) %>% 
  st_intersection(alb %>% st_transform(26918)) %>%
  filter(st_area(.) > set_units(100,'ft^2')) %>%
  mutate(nearstore = st_intersects(., alb.dist)
         ) %>%
  st_drop_geometry() %>%
#  group_by(nearstore) %>%
  summarise(pop = sum(value)) 


