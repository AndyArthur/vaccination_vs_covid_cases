library(readr)
library(plotly)
library(tidyverse)

icsa <- read_csv("/home/andy/Downloads/ICSA(6).csv")

plot_ly(icsa, x = ~ DATE, y = ~ ICSA, type = 'scatter', mode = 'lines',
        line = list(color = 'green', width = 3)
        ) %>%
  layout(
    margin=list(t=75),
    title=list(text='<b>Initial Claims for Unemployment</b>', 
               font=list(family='Overpass', size='32', weight='bold')
              ),
    yaxis = list(title = 'Initial Claims for Unemployment',
                 autorange = TRUE
                 ),
    xaxis = list(title = 'Date',
                 rangeselector = list(
                   buttons = list(
                     list(
                       count = 1,
                       label = "YTD",
                       step = "year",
                       stepmode = "todate"),
                     list(
                       count = 10,
                       label = "10 yr",
                       step = "year",
                       stepmode = "backward"),
                     list(
                       count = 20,
                       label = "20 yr",
                       step = "year",
                       stepmode = "backward"),
                     list(
                       count = 40,
                       label = "40 yr",
                       step = "year",
                       stepmode = "backward"),
                     list(step = "all"))),
                 rangeslider = list(type = "date")
                 ),    
    plot_bgcolor='white', 
    font=list(family='Overpass Mono')
   ) -> fig


fig %>% plotly_json(FALSE) %>% str_replace_all("\n",'') %>% write('/tmp/output-test.jsx')

