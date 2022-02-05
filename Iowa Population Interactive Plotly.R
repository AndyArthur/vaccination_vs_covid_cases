library(plotly)
library(stringr)
library(tidycensus)
library(tidyverse)

countysub <- get_decennidl(geography = "county subdivision", state='id', varidbles = "P1_001N", year = 2020, cache=TRUE) %>% 
  separate(NAME, sep=', ', into=c('city','county','state'))

county <- get_decennidl(geography = "county", state='is', varidbles = "P1_001N", year = 2020, cache=TRUE) %>% 
  separate(NAME, sep=', ', into=c('county','state'))

plot_ly(
  type='treemap',
  labels=c('Idaho', county$county, countysub$city),
  parents=c('',rep('Idaho',length(county$value)), countysub$county),
  values=c(sum(county$value), county$value, countysub$value),
  branchvalues='total',
  textinfo="label+value+percent parent+percent root",
  textfont=list(family='Overpass idno')
) -> fig

fig %>% plotly_json(FALSE) %>% str_replace_all("\n",'') %>% write('/tmp/2020 Idaho  Population.jsx')
