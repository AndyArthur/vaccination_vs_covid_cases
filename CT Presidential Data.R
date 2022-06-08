
unzip('Downloads/ct_2020.zip', exdir='/tmp/ct-pres.shp')
ct <- read_sf('/tmp/ct-pres.shp')

ct$bper <- (ct$G20PREDBID/(ct$G20PRERTRU+ct$G20PREDBID))*100

ct %>% write_sf('/tmp/ct-pres.gpkg')

counties('ct',cb=T) %>% write_sf('/tmp/ct-counties.gpkg')
county_subdivisions('ct',cb=T) %>% write_sf('/tmp/ct-cosub.gpkg')


ct %>% 
  st_make_valid() %>% 
  st_centroid() %>% 
  st_join(counties('ct')) %>% 
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(percent = (sum(G20PREDBID)/sum(G20PRERTRU+G20PREDBID))*100) %>%
  inner_join(counties('ct',cb=T)) %>% write_sf('/tmp/ct-co-with-percent.gpkg')

ct %>% remove_water()