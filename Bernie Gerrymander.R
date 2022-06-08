library(tidyverse)
library(sf)
library(tigris)
library(data.table)
library(tidycensus) 
library(janitor)
library(redist)
library(geomander)

rm(list=ls())

votes <- fread('2020vote_census_block.csv')
votes <- votes[COUNTY==1]

albany <- county_subdivisions('ny','albany') %>% 
  filter(NAMELSAD == 'Albany city') %>% 
  st_transform(3857)
  
votes <- clean_names(votes)
votes$blkid <- as.character(votes$blkid)

pop <-  get_decennial('block', state='ny', county='albany', variables = "P001001", geometry = T) %>% 
  st_transform(3857)

vpop <- pop %>% left_join(votes, by=c('GEOID'='blkid')) %>% 
  st_intersection(albany)

vpop$area <- st_area(vpop)

vpop <- vpop %>% filter(vpop$area > units::set_units(1000,'ft^2'))

adj <- adjacency(vpop)

vpop_map <- redist_map(vpop, total_pop = 'value', adj=adj, ndists=15, pop_tol = .01)
vpop_plans = redist_smc(vpop_map, nsims=2500)

vpop_map_ng <- vpop_map %>% st_drop_geometry()

bernie_plans <- c()

for (i in seq(1,2500)) {
  cbind(vpop_map_ng,dist=get_plans_matrix(vpop_plans)[,i]) %>% group_by(dist) %>% 
    summarize(sanders = sum(as.numeric(sanders),na.rm=T)/(sum(as.numeric(clinton),na.rm=T) + sum(as.numeric(sanders),na.rm=T)) ) -> med
  
  med$sanders %>% median(na.rm=T) %>% append(bernie_plans, .) -> bernie_plans
}

best_plan <- which.max(bernie_plans)


cbind(vpop_map,dist=get_plans_matrix(vpop_plans)[,best_plan]) %>% 
  group_by(dist) %>% 
  summarize(pop = sum(value), 
            sanders = sum(as.numeric(sanders),na.rm=T)/(sum(as.numeric(clinton),na.rm=T) + sum(as.numeric(sanders),na.rm=T))) %>%
 write_sf('/tmp/bernie2.gpkg')
# ggplot() + geom_sf(aes(fill=dist))

bernie_plans <- c()

for (i in seq(1,2500)) {
  plan <- cbind(vpop_map_ng,dist=get_plans_matrix(vpop_plans)[,i]) %>% 
    group_by(dist) %>%
    summarize(sanders = sum(as.numeric(sanders),na.rm=T)/(sum(as.numeric(clinton),na.rm=T) + sum(as.numeric(sanders),na.rm=T)) )
  
  plan %>%
    filter(sanders > .60) %>% 
    nrow() -> ct
  
  plan %>%
    filter(sanders > .55) %>% 
    nrow(.) * 0.1 + ct -> ct
  
  plan %>%
    filter(sanders > .50) %>% 
    nrow(.) * 0.01 + ct -> ct
  
    append(bernie_plans, ct) -> bernie_plans
}

best_plan <- which.max(bernie_plans)

cbind(vpop_map,dist=get_plans_matrix(vpop_plans)[,best_plan]) %>% 
  group_by(dist) %>% 
  summarize(pop = sum(value), 
            sanders = sum(as.numeric(sanders),na.rm=T)/(sum(as.numeric(clinton),na.rm=T) + sum(as.numeric(sanders),na.rm=T))) %>%
  write_sf('/tmp/bernie-60plusother.gpkg')
