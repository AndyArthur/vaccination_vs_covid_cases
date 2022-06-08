library(tidyverse)
rm(list=ls())

general <- read_csv('2020vote_vtd.csv')
general$GEOID <- as.character(general$GEOID)

vtd <- voting_districts('ny') %>% inner_join(general, by = c('GEOID20'='GEOID')) %>% st_transform('EPSG: 3857')

a22 <- read_sf('/home/andy/Documents/GIS.Data/2022-districts/2022-sd-may20-court-final.gpkg') %>% st_transform('EPSG: 3857')


nad <- a22 %>% 
  st_join(vtd %>% st_centroid()) %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
summarize(clinton = sum(CLINTON), sanders=sum(SANDERS), 
            total = sum(`TOT PRE DEM PRI 16`),
            cper = (clinton/total)*100, sper = (sanders/total)*100 )


a22 %>% inner_join(nad, by=c('id'='id')) %>% write_sf('/tmp/sanders-cd.gpkg')

vtd %>% write_sf('/tmp/sanders-vtd.gpkg')
