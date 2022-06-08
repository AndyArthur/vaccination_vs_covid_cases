library(tidyverse)
library(data.table)
library(tigris)
library(janitor)
rm(list=ls())

vtbk <- fread('2020vote_census_block.csv')

vtbk[COUNTY==1] 

ac$blkid <- as.character(ac$blkid)

acblk %>% inner_join(ac, by=c('GEOID20'='blkid')) %>% ggplot() + 
  geom_sf(aes(fill=dem20g1/sum(dem20g1,rep20g1,na.rm=T)))
