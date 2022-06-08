library(tidyverse)
library(classInt)
library(ggtext)

rm(list=ls())

zil <- read_csv('Downloads/County_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv')

zil$latest <- zil$`2022-04-30`

classes <- classIntervals(zil$latest, n = 10, style = "quantile", dataPercision=0)

zil <- zil %>% mutate(percent_class = cut(classes$var, breaks=classes$brks, include.lowest = T, 
                                               labels=scales::dollar(classes$brks[2:length(classes$brks)], accuracy = 1000)),
                      GEOID = str_c(StateCodeFIPS,MunicipalCodeFIPS))

st <- states(cb=T, resolution = '20m') %>% shift_geometry()
cos <- counties(cb=T, resolution = '20m') %>% shift_geometry()


cos %>%
  inner_join(zil, by=c('GEOID')) %>% 
  ggplot() + 
  geom_sf(aes(fill=percent_class), size=0.1, color='black') +
  geom_sf(data=cos, fill=NA, size=0.1, color='black') +
  geom_sf(data=st, fill=NA, size=0.4, color='black') +
  scale_fill_viridis_d(option='D') +
  coord_sf(expand=F) +
  labs(
    title = str_c('<span style="color: darkred; font-size: 36pt;">Zillow Home Value Index</span> <span style="font-size: 36pt; color: black">May 31, 2022</a>'),
    y = "",
    x = "",
    caption='https://www.zillow.com/research/data/',
    tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme_void() +
  theme(
    text=element_text(family='Open Sans',size=14),
    plot.title=element_textbox( hjust=0.5, face='bold',size=28, margin = margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(4,'cm'),
    legend.key.size = unit(0.5,'cm'),
    legend.position = 'top'
  )

fn <- 'zillo'
height <- 1300
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=height, units='px', dpi=150, device = grDevices::svg)
system(str_c('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz'))

