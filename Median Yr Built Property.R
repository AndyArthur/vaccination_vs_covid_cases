library(RPostgreSQL)
library(tidyverse)
library(sf)
library(tigris)

rm(list=ls())
con <- DBI::dbConnect(RPostgres::Postgres(), dbname='gis', host='localhost', 
                      port=5432, user='postgres', password='farmlove')


str_c("select yr_blt, st_transform(shape, 3857) as geom FROM nytax_ctr_pt WHERE yr_blt>0") -> sql
yrblt <- st_read(con, query=sql)
  
blocks('ny') %>% 
  st_transform(3857) %>% 
    st_intersection(yrblt) %>% 
    st_drop_geometry() %>% 
    group_by(GEOID20) %>%
    summarize(blockyr = median(yr_blt)) %>% 
    inner_join(mb, bb) %>% 
    write_sf('/tmp/median-block.gpkg')