library(tidyverse)
library(ggtext)
library(lubridate)

rm(list=ls())

wx <- read_csv('~/Downloads/3059447.csv')

wx %>% select(DATE, TMIN) %>%
  filter(year(DATE) > 2002) %>%
  group_by(TMIN) %>%
  summarise(n = n()) %>%
  filter(TMIN < 1) %>%
  ggplot() + geom_col(aes(x=TMIN, y=n, fill=TMIN)) +
  geom_text(aes(label=n, x=TMIN, y=n, color=TMIN), vjust=0, nudge_y = 0.25, size=5, fontface='bold') +
  scale_x_continuous(breaks=seq(-30,0,1), name='Low Temperature') +
  scale_y_continuous(breaks=seq(0,70,2), limits=c(0,20), name='Times Since January 1, 2002') +
  scale_fill_gradient(high='blue', low='purple') +
  scale_color_gradient(high='blue', low='purple') +
  coord_cartesian(expand=F) +
  theme_bw() +
  labs(
    title = '<span style="font-size: 14pt">Nights Since January 2002 that have been</span>  <b style="color: blue">below zero or <em>colder</em></b>',
    caption='https://www.ncdc.noaa.gov/',
    tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme(
    text=element_text(family='Noto Sans',size=14),
    axis.ticks = element_blank(),
    axis.text = element_text(face='bold'),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=32, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0),
    plot.caption=element_text(size=10),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'none',
  ) 

fn <- str_c('days-below-zero')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

library(gt)
wx %>% select(DATE, TMIN) %>%
  filter(year(DATE) > 2001, TMIN < -9) %>%
  arrange(rev(DATE)) %>%
  #slice_min(TMIN, n=20) %>%
  gt() %>%
  tab_header('Albany Aiport - Nights Negative 10 or Colder', 'Negative 13 is about as cold as it gets these days.') %>%
  tab_footnote(html('Andy Arthur, 2/4/23.<br /><em>Data Sources:</em> National Weather Service Records, <br />January 2002 to Present, Albany Airport')) %>%
  opt_stylize(style=3) %>%
  opt_row_striping() %>%
  fmt_date(1,date_style="yMMMEd") #%>%
  gtsave('/tmp/coldest.html')
