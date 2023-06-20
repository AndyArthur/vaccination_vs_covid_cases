library(tidyverse)
library(sf)
library(suncalc)
library(tigris)

rm(list=ls())

usco <- counties(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('HI','AK','PR')) %>%
  rmapshaper::ms_simplify()

rclr <- RColorBrewer::brewer.pal(8, 'YlOrRd') %>% rev 

usst <- states(cb=T, resolution = '20m') %>%
  filter(!STUSPS %in% c('HI','AK', 'PR'))

usco.centroid <- usco %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  transmute(GEOID, lat = st_coordinates(.)[,2],  lon = st_coordinates(.)[,1]) %>%
  st_drop_geometry()

June.21.LOD = getSunlightTimes(
  data = bind_cols(usco.centroid, date=rep(as.Date('2023-06-21'), nrow(usco.centroid))),
  keep = c('sunrise','sunset')
) %>%
  bind_cols(GEOID = usco.centroid$GEOID, .) %>%
  mutate(LOD = (sunset - sunrise) %>% as.numeric()) %>%
  select(GEOID, LOD) 

usco %>%
  left_join(June.21.LOD) %>%
  ggplot() + geom_sf(aes(fill=LOD), linewidth=0.05, color='white') +
  geom_sf(data=usst, fill=NA, linewidth=0.6, color='white') +
  scale_fill_stepsn(colours = rclr, name='hours of daylight', breaks=seq(14, 16, 0.5)) +
  coord_sf(crs = 5070, expand=F) + 
  theme_void() + 
  labs(title = str_c('<span style="font-size: 35pt; color: ',rclr[3],'">Daylight</span> <span style="font-size: 30pt">on the first day of </span> ',
                     '<span style="font-size: 35pt; color: ',rclr[7],'">Summer</span>',
                     '<br />The farther north you get the more daylight you have on first day of summer.'),
       y = "",
       x = "",
       tag=paste('rSuncalc for the centroid of each county.<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m-%-d-%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=12, width=0.8, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0.03),
    legend.position = c(0.8,1.04),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.title.align = 0.5,
    legend.title = element_text(face='italic', size=10)
  ) +
  guides(fill = guide_colorsteps(title.position = 'bottom'))

fn <- str_c('Daylight-on-the-First-Day-of-Summer')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1280, units='px', dpi=130)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1280, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

source('upload-svg.R', local = T)