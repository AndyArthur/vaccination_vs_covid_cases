library(tidycensus)
library(tidyverse)
library(sf)

# shapefile of Albany Wards, project to web mercator 3857
# calculate area of each ward, load Ward field as numeric
# hide extra fields in shapefile
wards <- read_sf('/tmp/Albany Wards.gpkg') %>%
  st_transform(3857) %>%
  mutate(ward_area = st_area(.),
         Ward = as.numeric(Ward)) %>%
  dplyr::select(Ward, ward_area)

# get median household income, re-project
# to web mercator 3857
income<- get_acs(
  geography = "tract",
  state='ny',
  county='Albany',
  variables = "B19013_001",
  year = 2020,
  survey = "acs5",
  geometry = T
) %>% 
  st_transform(3857)


# intersect income against wards 
# calculate percent of ward's area in each census tract
# group back together as a single ward
# create a weighted-mean of median household inco
# rejoin back together with original ward shapefile
income %>% 
  st_intersection(wards) %>%
  mutate(percent_of_ward = (st_area(.) / ward_area) %>% units::drop_units() ) %>%
  st_drop_geometry() %>%
  group_by(Ward) %>%
  summarise(estimate = weighted.mean(estimate, percent_of_ward, na.rm=T)) %>%
  inner_join(wards, ., by=c('Ward')) 
