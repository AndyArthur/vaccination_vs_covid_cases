library(tidyverse)
library(arcpullr)

orthos <- arcpullr::get_spatial_layer('https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/arcgis/rest/services/NYS_Orthoimagery_All_Years_Index/FeatureServer/0/')

nyco <- tigris::counties('ny') %>% st_transform(3857) %>%
  mutate(area = st_area(.))

orthos %>% st_transform(3857) %>% 
  st_make_valid() %>%
  st_intersection(nyco) %>%
  filter(Year != 1996, units::drop_units(st_area(.)/area) > .5) %>%
  st_drop_geometry() %>%
  group_by(NAME, Year) %>%
  summarise(Year = first(Year)) %>%
  ungroup() %>%
  group_by(NAME) %>%
  summarise(Year = paste(Year, collapse=','), CT = n()) %>% 
  print(n=80)
 