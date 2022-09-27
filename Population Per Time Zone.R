library(sf)
library(tidyverse)
library(tidycensus)
rm(list=ls())

tz <- read_sf('~/Documents/GIS.Data/natural_earth_vector/10m_cultural/ne_10m_time_zones.shp') %>% st_transform(5070)

pop <- counties()  %>% st_transform(5070)

cv <- load_variables('pl', year=2020)

pop <- get_decennial('county', variables = 'P1_001N', geometry = T, year=2020) %>% st_transform(5070)

pop <- st_intersection(pop, tz)

pop <- pop %>% shift_geometry()

pop %>% group_by(tz_name1st) %>% 
  mutate(sumpop = sum(value), per=sumpop/sum(pop$value)) %>% group_by(tz_name1st) %>% summarise(sumpop = first(sumpop), per = first(per)) -> tz

tz %>% rmapshaper::ms_simplify() -> tz

st <- states(cb=T, resolution = '20m') %>% shift_geometry()

vi <- viridis::cividis(5)

ggplot(tz) + 
  geom_sf(aes(fill=sumpop), size=0.2) + 
  geom_sf(data=st, fill=NA) +
  geom_sf_label(aes(label=str_c(str_replace(tz_name1st,'_',' '), '\n',scales::comma(sumpop),'\n',scales::percent(per))), alpha=0.8) +
  scale_fill_viridis_c() +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('How Many People Reside In Each  <span style="color: ',vi[1],'; font-size: 46pt">Time Zone</span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br /><em>2020 US Census - Natural Earth Time Zones'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(halign=0.5, size=28, face='bold'),
    plot.subtitle=ggtext::element_textbox_simple(minheight = 0, height=0),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.8,0.03),
    legend.position = 'none'
  ) 

fn <- str_c('tz-pop')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))