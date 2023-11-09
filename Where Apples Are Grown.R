library(tidyverse)
library(CropScapeR)
library(sf)
library(terra)
library(ggtext)
data <- GetCDLData(aoi = '36', year=2022, type='f', save_path = '/tmp/22nycrop.tif') == 68
#data <- rast('/tmp/22nycrop.tif') == 68

nys  <- tigris::states(cb=T) %>% filter(NAME == 'New York') %>% st_transform(5070)

st_make_grid(nys, n = c(80,80), square = FALSE, flat_topped = FALSE) %>% 
  st_intersection(nys) %>% st_transform(5070) %>% st_as_sf() %>% dplyr::filter(st_area(.) > units::set_units(0.25,'km^2')) -> nygrid

exactextractr::exact_extract(data, nygrid, fun='frac') -> pumpkin.cover

nyco <- tigris::counties('ny', cb=T)

nygrid %>% cbind(pumpkin.cover) %>%
  mutate(acres = (frac_1*st_area(.)) %>% units::set_units('acres') %>% units::drop_units()) %>% 
  ggplot() + geom_sf(aes(fill=acres), linewidth=0) + 
  geom_sf(data=nyco, fill=NA, color='darkred', linetype='dotted') +
  scale_fill_steps(low='white', high='darkred', trans='log10', limits=c(1,1000), na.value = 'white', n.breaks=8, labels=scales::label_comma(), name='Acres') +
  coord_sf(expand=F, crs=3857) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 58pt; color: lightyellow">Apples 🍎</span><br /><br />Grown in New York State (2022)</em>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: Cropscape (2022)'),
       fill = "")  +
  theme(
    legend.position = c(0.97,0.5),
    legend.justification = c(1,0.5),
    text= element_text(family='Roboto Condensed',size=14, color='white'),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=35, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "darkred", color="darkred"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=14,hjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.size  = unit(1.5, 'cm'),
  ) +
  guides( fill = guide_legend(
    title.theme = element_text(face='bold', size=18, color='lightyellow', hjust = 0.5, family='Roboto Condensed'),
    label.theme = element_text(color='white', size=14, family='Roboto Condensed'),                       
                              label.hjust = 1, 
    override.aes = list(size=6, shape='diamond' )))

fn <- 'Apples_Grown_in_NYS'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=140)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=140, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
source('upload-svg.R', local=T)
