library(tidyverse)

tmpF <- tempfile()
download.file('http://wwe1.osc.state.ny.us/localgov/findata/counties/levelone21.xlsx', tmpF)

countyl1 <- readxl::read_excel(tmpF, skip=4)

nyco <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Civil_Boundaries/FeatureServer/3')

nyco

countyl1

nyco %>%
  mutate(`Muni Code` = str_pad(SWIS, 10, side='right', pad='0')) %>%
  inner_join(countyl1)
         

nyco