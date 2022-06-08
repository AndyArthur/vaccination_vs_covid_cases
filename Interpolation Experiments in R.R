library(tidyverse)
library(tigris)
library(sf)
library(gstat)
library(raster)
library(terra)
rm(list=ls())

general <- read_csv('2020vote_vtd.csv')
general$GEOID <- as.character(general$GEOID)

beth <- county_subdivisions('ny') %>% 
  filter(NAME == 'Bethlehem') %>%
  st_transform(3857)

vtd <- voting_districts('ny') %>% 
  filter(COUNTYFP20 == '001') %>%
  inner_join(general, by = c('GEOID20'='GEOID')) %>% 
  st_centroid() %>%
  st_transform(3857) %>%
  st_intersection(beth, .)

vtd <- vtd %>% mutate(biden = DEM20G6/(DEM20G1+REP20G1))


vtd2 <- voting_districts('ny') %>% 
  filter(COUNTYFP20 == '001') %>%
  inner_join(general, by = c('GEOID20'='GEOID')) %>% 
  st_transform(3857) %>%
  st_intersection(beth, .) %>% 
  mutate(biden = DEM20G6/(DEM20G1+REP20G1))

ggplot(vtd2) + geom_sf(aes(fill=biden),size=.1) + scale_fill_gradient2(midpoint = 0.5)

library(gstat)
# create bethelhem blocks, suppress blocks that are less then 1,000 ft
# as they are clipping erros
bb <- blocks('ny','albany') %>% st_transform(3857) %>% st_intersection(beth,.)
bb <- bb %>% filter(st_area(.) > units::set_units(1000,'ft^2'))

# interpolate election results, 50 square meters
bethr <- raster(beth, res=100)
gs <- gstat(formula = biden~1, locations = vtd, set=list())
nn <- interpolate(bethr, gs)

# extract by block
bbr <- terra::extract(nn, bb, fun=median)
cbind(bb, bbr) %>% ggplot() + geom_sf(aes(fill=bbr),size=.1) + scale_fill_gradient2(midpoint=0.5)

cbind(bb, bbr) %>% write_sf('/tmp/beth.gpkg')

# mask the raster
rb <- mask(nn, beth)
writeRaster(rb, '/tmp/output.tif', options=c("COMPRESS=LWZ", "TFW=YES"))

# convert to spatial pixel dataframe to map with ggplot
rbdf <- as(rb, "SpatialPixelsDataFrame") %>% as.data.frame()

# display raster
ggplot(beth) + geom_sf() + geom_raster(data=rbdf, aes(x,y, fill=var1.pred)) + 
  scale_fill_gradient2(midpoint = 0.5) 
