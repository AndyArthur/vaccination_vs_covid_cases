library(readr)
library(plotly)
library(tidyverse)
rm(list=ls())
apu <- read_csv("/home/andy/Downloads/APU0000703112.csv")

apu$APU0000703112_PC1<- as.numeric(apu$APU0000703112_PC1)


plot_ly(apu, x = ~ DATE, y = ~APU0000703112_PC1, type = 'scatter', mode = 'lines',
        line = list(color = 'brown', width = 1)
        ) %>%
  layout(
    margin=list(t=75),
    title=list(text='Change in Price of <b style="color: brown">One-Lb Ground Beef</b> from Previous Year', 
               font=list(family='Noto Sans', size='32', weight='normal'),
               margin=c(0,0,10,0)
              ),
    yaxis = list(title = 'Change from Year Ago'
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
                       count = 5,
                       label = "5 yr",
                       step = "year",
                       stepmode = "backward"),
                     list(
                       count = 10,
                       label = "10 yr",
                       step = "year",
                       stepmode = "backward"),
                     list(step = "all"))),
                 rangeslider = list(type = "date")
                 ),    
    plot_bgcolor='white', 
    font=list(family='Overpass Mono')
   ) -> fig


fig %>% plotly_json(FALSE) %>% str_replace_all("\n",'') %>% write('/tmp/mortage-rate.jsx')

