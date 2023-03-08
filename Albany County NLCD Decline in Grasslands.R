library(sf)
library(tigris)
library(tidyverse)
library(terra)
rm(list=ls())

acty <- counties('ny') %>% filter(NAME == 'Albany') %>% st_transform(5070)

nlcd19 <- FedData::get_nlcd(acty, '2019alb', 2019)
nlcd01 <- FedData::get_nlcd(acty, '2001alb', 2001)

grasschg <- (nlcd19 == 71 | nlcd19 == 81) - (nlcd01 == 71 | nlcd01 == 81)

grasschg <- grasschg %>% mask(acty)

nlcd.poly <- as.polygons(rast(grasschg))

nlcd.poly <- nlcd.poly %>% st_as_sf() %>% st_transform(3857) %>%
  filter(layer != 0) %>%
  st_buffer(units::set_units(100, 'feet')) %>%
  mutate(
    layer = 
      layer %>%
      as.numeric %>%
      case_match(
       -1 ~ 'Loss',
       1 ~ 'Increase'
        )
    ) 

cat.cos <- county_subdivisions('ny', 'albany', cb=T)
cat.cty <- acty

ggplot() + geom_sf(data=nlcd.poly, aes(fill=layer), linewidth=0) +
  geom_sf(data=cat.cos, fill=NA) +
  geom_sf(data=cat.cty, fill=NA, linewidth=1) +
  scale_fill_manual(values=c('darkgreen','orange'), name = '')  +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="">Change in <br /><b style="font-size: 43pt; color: darkgreen">Grasslands and Pasture 🐦</b><br /> in Albany County ',
                     'from 2001 to 2019'),
       tag = 'Andy Arthur - March 8, 2023<br />Note: Area of pasture/grassland buffered by 100 ft to improve readability.'
      )  +
  theme(
    text= element_text(family='Roboto Condensed',size=13),
    plot.title=ggtext::element_textbox(hjust=0, halign=0, face='bold', height=0),
    plot.tag=ggtext::element_textbox(hjust=1, halign=1, height=0, valign=0),
    plot.tag.position = c(1,0),
    plot.background = element_rect(fill = "mintcream", color="mintcream"),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    legend.position = c(0,0.85),
    legend.text = element_text(size=16, face='bold'),
    legend.direction = 'horizontal',
    legend.justification = 'left',
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(2,'cm')
  ) 

fn <- str_c('acty-grass')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1700, units='px', dpi=150)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1700, units='px', dpi=150, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

