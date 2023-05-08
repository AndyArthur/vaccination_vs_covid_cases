library(vroom)
library(tidyverse)
library(sf)
library(gt)

rm(list = ls())
gnis <- vroom("/media/hd2/draft.redistricting/NationalGNIS.zip")

asd <- county_subdivisions('ny') %>% st_transform(5070)

gnis %>%
  filter(STATE_ALPHA == "NY", COUNTY_NAME == 'Schoharie', FEATURE_CLASS == "Summit" ) %>%
  st_as_sf(coords = c("PRIM_LONG_DEC", "PRIM_LAT_DEC"), crs = 4326, agr = "constant") %>%
  st_transform(5070) %>%
  st_intersection(asd) %>%
  #slice_max(ELEV_IN_FT, n=20) %>%
  arrange(-ELEV_IN_FT) %>%
  mutate(Tallest_Peak = row_number()) %>%
  st_transform(4326) %>%
  transmute(
    `Tallest Summit` = Tallest_Peak,
    Name = FEATURE_NAME,
    Elevation = ELEV_IN_FT,
    Town = NAME, 
    Latitude = st_coordinates(.)[,2],
    Longtitude = st_coordinates(.)[,1]
  ) %>%
  st_drop_geometry() %>%
  gt() %>%
  fmt_integer(3) %>%
  gtExtras::gt_theme_538() %>%
  tab_footnote(html('Andy Arthur, 4/27/23. <span style="float: right"><em>Data Source:</em>
                    USGS GNIS.')) %>%
  tab_options(
    footnotes.font.size = 12,
    heading.align = 'center') %>%
  gtsave('/tmp/warm.png', vwidth=1920, expand=c(5,15,5,15))
  as_raw_html(inline_css = F) %>% write_file('/tmp/hom.html') 
