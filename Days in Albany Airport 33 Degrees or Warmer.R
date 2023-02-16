library(tidyverse)
library(ggtext)
library(lubridate)

rm(list=ls())

wx <- read_csv('~/Downloads/3059447.csv')

wx %>% filter(month(DATE) == 1) %>%
  filter(TMAX > 32) %>%
  group_by(Year = year(DATE)) %>%
  summarise(Days = n()) %>%
  add_row(Year = 2023, Days = 29) %>% 
  ggplot() + geom_col(aes(Year, Days, fill=Days)) +
  scale_fill_viridis_c() +
  scale_y_continuous(breaks=seq(0, 29, 1), minor_breaks = seq(0, 29, 1)) +
  scale_x_continuous(breaks=seq(1935, 2025, 5)) +
  coord_cartesian(expand=F) +
  theme_bw() +
  labs(
    title = '<span style="font-size: 14pt">January Days that are</span> <b>33 degrees or warmer</b> <span style="font-size: 14pt">at the Albany Airport</span>',
    y='Number of January Days', 
    x='Year',
    caption='https://www.ncdc.noaa.gov/',
    tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme(
    text=element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=32, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0),
    plot.caption=element_text(size=10),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'none',
  ) 

fn <- str_c('albany-airport-warm')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

