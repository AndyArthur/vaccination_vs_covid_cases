library(tidyverse)
library(lubridate)
library(gt)
library(plotly)

rm(list=ls())

wx <- read_csv('~/Downloads/3059447.csv')

wx %>% 
  filter(month(DATE) == 5, year(DATE) > 2002) %>%
  transmute(HighTemp = floor(TMAX/5)*5, Days = sum(n())) %>%
  group_by(HighTemp) %>%
  summarise(Days = n()) %>%
  ggplot() + geom_col(aes(HighTemp, Days, fill=HighTemp)) +
  scale_y_continuous(breaks=seq(0,200,10)) +
  scale_x_continuous(breaks=seq(0,100,5)) +
  scale_fill_gradientn(name = "", colors = c("#DC143C", "pink", "violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill", limits = c(-35,115), seq(-30,115,10)) +
  theme_minimal() +
  coord_cartesian(expand=F) +
  labs(
    title = 'High Temperature in February, 2002-2022',
    y = 'Days in February',
    x = ''
  )+
  theme(
    legend.position = 'none',
    text = element_text(family='Roboto Condensed', size=18,hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face='bold')
  ) -> fig
  

fig %>% plotly_json(FALSE) %>% str_replace_all("\n",'') %>% write('/tmp/covid-19-positivity.jsx')


fig

wx %>% 
  filter(month(DATE) == 2) %>%
  mutate(Above40 = TMAX >= 40) %>%
  group_by(consecutive_id(Above40)) %>%
  summarise(days = n(), Above40 = first(Above40), start = first(DATE), end = last(DATE), maxTemp  = max(TMAX)) %>%
  filter(Above40) %>%
  select(Days = days, Start = start, End = end, MaxTemp = maxTemp) %>%
  slice_max(Days, n=10)
