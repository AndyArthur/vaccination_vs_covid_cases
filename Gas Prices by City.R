library(rvest)
library(tidyverse)
library(tigris)
library(zoo)
library(classInt)
library(janitor)
library(ggtext)
library(lubridate)
library(raster)
library(gstat)

rm(list=ls())
url <- 'https://data.ny.gov/api/views/nqur-w4p7/rows.csv?accessType=DOWNLOAD&sorting=true'

gas <- read_csv(url)

gas$Date <- as.Date(gas$Date, format='%m/%d/%Y') 

csp <- county_subdivisions('ny', cb=T) %>% 
  st_centroid()

cos <- counties('ny', cb=T)

gaspt <- gas %>% 
  pivot_longer(cols = 2:ncol(.)) %>% 
  mutate(name = str_replace(name, ' Average.*',''), 
         name = str_replace(name, ' City',''),
         name = paste(name, 'city'),
         name = str_replace(name, 'New York city','Manhattan borough'),
         name = str_replace(name, 'Dutchess city','Poughkeepsie city'),
         name = str_replace(name, 'Nassau city','Hempstead town'),
         ) %>% 
  filter(Date == max(Date)) %>%
  right_join(csp, ., by=c('NAMELSAD'='name')) %>%
  drop_na() %>%
  st_crs(3857)
  #%>%
  ggplot() +
  geom_sf(data=cos, fill=NA) +
  geom_sf()
  
grast <- raster(cos %>% group_by() %>% summarize(), res=100)
gs <- gstat(formula = value~1, locations = gaspt, set=list())
nn <- interpolate(grast, gs)

plot(nn)

rbdf <- as(nn, "SpatialPixelsDataFrame") %>% as.data.frame()

# display raster
ggplot() +  geom_raster(data=rbdf, aes(x,y, fill=var1.pred)) 

