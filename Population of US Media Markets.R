# load libraries
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)

rm(list=ls())

acs <- map_dfr(
  state.abb %>% setdiff(c('AK','HI')),
  ~get_acs("tract", survey='acs5', var='B01001_001', state=., cache_table = T,
               geometry = T,
               year = 2021)
  )

acs <- st_transform(acs, 5070)

natdma <- read_sf('/home/andy/Documents/GIS.Data/media-market-dma/NatDMA.shp') %>% rmapshaper::ms_simplify() %>% st_transform(5070)
natdma <- natdma %>% filter(NAME!='National')

acs %>% st_centroid() %>% st_intersection(natdma) -> acsdma

usst <- states(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('AK','HI','PR'))

acsdma %>% st_drop_geometry() %>%
  group_by(NAME.1) %>%
  summarise(estimate = sum(estimate)) %>%
  inner_join(natdma, ., join_by(NAME == NAME.1)) %>% 
  rmapshaper::ms_simplify() %>%
  ggplot() + geom_sf(aes(fill=estimate), color='gray90') +
  scale_fill_viridis_b(labels=scales::label_comma(), trans='log10', n.breaks=8, name='') +
  geom_sf(data=usst, fill=NA, linewidth=0.3) +
  theme_bw() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 45pt">Population of Media Markets</span>'),
      tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%d/%y"),
                 '<br />Source: 2021 ACS Estimated Population (Census Tracts). Media Markets, drive.google.com/file/d/1yuJRTJNNmv6A66Jr7qCgOa-Uz-cA_JWZ/view?usp=sharing'),
      )  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=12, width=0.7, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555',  halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = 'bottom',
    legend.position = c(1,1.04),
    legend.justification = 'right',
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
  ) 

fn <- str_c('mm-pop')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1350, units='px', dpi=130, device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1350, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


###########

usco <- counties(resolution = '20m', cb=T) %>% st_transform(5070) %>%
  mutate(CoStr = str_c(NAMELSAD,', ', STUSPS))

usco %>% st_intersection(natdma) %>%
  st_drop_geometry() %>%
  group_by(NAME.1) %>%
  summarize(Counties = paste(sort(CoStr), collapse='; ')) ->
  mmcos

library(gt)

acsdma %>% st_drop_geometry() %>%
  group_by(NAME.1) %>%
  summarise(estimate = sum(estimate)) %>%
  left_join(mmcos) %>%
  slice_max(estimate, n=10) %>% 
  transmute(Market = NAME.1, Population = estimate, `Counties in part or whole` = Counties) %>%
  gt() %>%
  fmt_integer(2) %>%
  opt_stylize() %>%
  tab_style(style=list(cell_text(size = 'x-small')), locations = cells_body(3)) %>%
  gtsave('/tmp/largestmediamarket.html')


nys <- states() %>% filter(STUSPS == 'NY') %>% st_transform(5070)

nys %>% st_join(natdma) %>% st_drop_geometry() %>% select(NAME=NAME.y) -> nymm

acsdma %>% st_drop_geometry() %>%
  mutate(nyest = ifelse(substr(GEOID,0,2) == '36', estimate, 0)) %>%
  group_by(NAME.1) %>%
  summarise(estimate = sum(estimate),
            nypop = sum(nyest),
            inny = sum(nyest) / sum(estimate)
            ) %>%
  left_join(mmcos) %>%
  right_join(nymm, join_by(NAME.1 == NAME)) %>% 
  filter(inny > 0) %>%
  transmute(Market = NAME.1, Population = estimate, `Population in NYS` = nypop, `% in NYS` = inny, `Counties in part or whole` = Counties) %>%
  gt() %>%
  fmt_integer(2:3) %>%
  fmt_percent(4, decimals = 0) %>%
  opt_stylize() %>%
  tab_style(style=list(cell_text(size = 'x-small')), locations = cells_body(5)) %>%
  gtsave('/tmp/nymediamarket.png')
