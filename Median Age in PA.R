library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)

us_median_age <- get_acs(
  state='pa',
  year=2020,
  geography = "tract",
  variables = "B01002_001",
  survey = "acs5",
  geometry = T, 
)

us_median_age %>% write_sf('/tmp/pa-age.gpkg')

get_acs(
  state='pa',
  year=2020,
  geography = "county",
  variables = "B01002_001",
  survey = "acs5",
  geometry = T, 
) %>% write_sf('/tmp/paco.gpkg')
