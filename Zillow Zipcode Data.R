library(tigris)
library(tidyverse)
rm(list=ls())

zip <- zctas(year=2019, cb=T)

zname <- read_csv('Documents/GIS.Data/zip-code-names.csv')

home <- read_csv('Downloads/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv')
zip %>% 
  inner_join(home, by=c('GEOID10'='RegionName')) %>% 
  inner_join(zname, by=c('GEOID10'='Zip Code')) %>%
  write_sf('/tmp/singlefamily.gpkg')
