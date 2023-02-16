library(sf)
library(tigris)
library(tidyverse)
library(terra)
rm(list=ls())

nlcd <- terra::rast('~/Documents/GIS.Data/agriculture/nlcd2019_ny.tif')

nlcd10 <- nlcd %>% aggregate(fact=30, fun='median')
nlcd.poly <- as.polygons(nlcd10)
nlcd.poly <- nlcd.poly %>% st_as_sf() %>% st_transform(3857)

nlcd.poly <-nlcd.poly %>% 
  left_join(FedData::nlcd_colors(), join_by(`NLCD Land Cover Class` == ID)) %>% 
  mutate(Percent = (st_area(.) / sum(st_area(.))) %>% units::drop_units(),
         Area = st_area(.) %>% units::set_units('mi^2') %>% units::drop_units() %>% scales::comma(accuracy = 1),
         Label = str_c(Class, ', ', scales::percent(Percent, accuracy=0.1), ', ', Area, ' sq. mi')) %>%
  filter(Percent > 0.0005) %>%
  drop_na(Label)

cat.cos <- county_subdivisions('ny', cb=T)  %>% rmapshaper::ms_simplify()
cat.cty <- counties('ny', cb=T) %>% rmapshaper::ms_simplify()

ggplot() + geom_sf(data=nlcd.poly, aes(fill=`Color`), linewidth=0) +
  geom_sf(data=cat.cos, fill=NA) +
  geom_sf(data=cat.cty, fill=NA, linewidth=0.6) +
  scale_fill_identity(breaks = nlcd.poly$Color, labels = nlcd.poly$Label, guide = "legend", name='Land Cover') +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="">Land Cover in the <br /><b style="font-size: 53pt; color: darkgreen">New York State</b></span><br />',
                     '2019 National Land Cover Dataset<br />Andy Arthur, 2/4/23'),
      )  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox(hjust=0, halign=0, face='bold', height=0, size=26),
    plot.background = element_rect(fill = "mintcream", color="mintcream"),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    legend.position = c(0, 0),
    legend.direction = 'vertical',
    legend.justification = c(0,0),
    legend.key.height = unit(0.7,'cm'),
    legend.key.width = unit(1.5,'cm')
  ) +
  guides(fill = guide_legend(ncol=2, override.aes = list(color='mintcream', linewidth=1, size=1)))

fn <- str_c('nys-landcover')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

