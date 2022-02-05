library(plotly)
library(stringr)
library(tidycensus)
library(tidyverse)

countysub <- get_decennial(geography = "county subdivision", state='mo', variables = "P1_001N", year = 2020, cache=TRUE) %>% 
  separate(NAME, sep=', ', into=c('city','county','state'))

county <- get_decennial(geography = "county", state='mo', variables = "P1_001N", year = 2020, cache=TRUE) %>% 
  separate(NAME, sep=', ', into=c('county','state'))

plot_ly(
  type='treemap',
  labels=c('Missouri', county$county, countysub$city),
  parents=c('',rep('Missouri',length(county$value)), countysub$county),
  values=c(sum(county$value), county$value, countysub$value),
  branchvalues='total',
  textinfo="label+value+percent parent+percent root",
  textfont=list(family='Overpass Mono')
) -> fig

fig %>% plotly_json(FALSE) %>% str_replace_all("n",'') %>% write('/tmp/2020 Missouri  Population.jsx')
