library(tidycensus)
library(sf)
library(tigris)
library(tidyverse)

pop2020 <- get_decennial(geography = "block group", state='ny', county='Albany', variables = "P1_001N", 
                         year = 2020, cache=TRUE, geometry = F)

mtbg <- block_groups(state='ny',  county='Albany', year=2020) 

mtbg %>% inner_join(pop2020, by='GEOID') %>%
  mutate(Population_per_Sq_Mi = value/(ALAND/2589988.1103)  ) %>% 
  st_simplify(preserveTopology = T, dTolerance = 10) %>%
  st_transform('EPSG: 26918') %>% 
  write_sf('/tmp/albany.gpkg')
