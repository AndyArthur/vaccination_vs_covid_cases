library(tidycensus)
library(tidyverse)
library(terra)
library(FedData)
library(sf)
library(tigris)
library(units)
rm(list=ls())

cos <- counties('ny') %>% st_transform(5070)

nlcd19 <- rast('Documents/GIS.Data/agriculture/nlcd2019_ny.tif')
nt <- terra::extract(nlcd19, vect(cos), factors=F)

pxSize <- res(nlcd19)[1]*res(nlcd19)[2] %>% units::set_units(m^2) %>% units::set_units(mi^2)

nt %>% 
  as.data.frame() %>%
  count(`ID`, `NLCD Land Cover Class`) %>% 
  arrange( `NLCD Land Cover Class`) %>%
  mutate('NLCD Land Cover Class' = as.character(`NLCD Land Cover Class`)) %>%
  inner_join(pal_nlcd(), by=c('NLCD Land Cover Class'='code')) %>%
  select(ID, class, n) %>%
  pivot_wider(names_from = `class`, values_from=n, values_fn=sum) %>%
  mutate(across(2:ncol(.),  ~ . * pxSize)) %>%
  rowwise() %>%
  mutate(total =  sum(across(2:ncol(.)),na.rm=T))  %>%
  cbind(cos, .) %>%
  select(NAME, total, ncol(cos):ncol(.)) %>%
  drop_units() %>% 
  st_drop_geometry() %>%
  arrange(NAME) %>%



