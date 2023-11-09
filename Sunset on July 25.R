library(tidyverse)
library(sf)
library(suncalc)
library(tigris)

rm(list=ls())

tz <- read_sf('~/Documents/GIS.Data/CONUS Time Zones.gpkg') %>%
  st_transform(5070)


usco <- counties(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('HI','AK','PR')) %>%
  rmapshaper::ms_simplify()

usco.centroid <- usco %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  transmute(GEOID, lat = st_coordinates(.)[,2],  lon = st_coordinates(.)[,1]) %>%
  st_transform(5070) %>%
  st_join(tz) %>%
  st_drop_geometry() 

ss = getSunlightTimes(
  data = bind_cols(usco.centroid, date=rep(as.Date('2023-11-08'), nrow(usco.centroid))),
  keep = c('sunset')
) %>%
  bind_cols(GEOID = usco.centroid$GEOID, ., offset=usco.centroid$`timezones-data_offset_dst`) %>%
  select(GEOID, sunset, offset)

rclr <- viridis::inferno(8)

usst <- states(cb=T, resolution = '20m') %>%
  filter(!STUSPS %in% c('HI','AK', 'PR'))


usco %>%
  left_join(ss) %>%
  mutate(sunset = cut(sunset + offset + 4*60*60, breaks = seq(as.POSIXct('2023-11-08 00:00'), as.POSIXct('2023-11-08 24:00'), '10 mins'),
                      labels = format(seq(as.POSIXct('2023-05-22 00:00'), as.POSIXct('2023-05-22 23:50'), '10 mins'),'%I:%M %P'))
         ) %>%
  ggplot() + geom_sf(aes(fill=sunset), linewidth=0.1, color='lightyellow') +
  geom_sf(data=usst, fill=NA, linewidth=0.8, color='white') +
  scale_fill_viridis_d(option = 'B') +
  coord_sf(crs = 5070, expand=F) + 
  theme_void() +
  labs(title = str_c('<span style="font-size: 35pt; color: ',rclr[3],'">Local Time Sunset</span> <span style="font-size: 30pt">on</span> ',
                     '<span style="font-size: 35pt; color: ',rclr[7],'">November 8, 2023</span>'
                     ),
       y = "",
       x = "",
       tag=paste('rSuncalc for the centroid of each county.<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m-%-d-%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0.5, halign=0.5, face='bold', size=45, width=1, height=0, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0.03),
    legend.position = c(1,0),
    legend.justification = c(1,0),
    legend.direction = 'verical',
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.title.align = 0.5,
    legend.title = element_text(face='italic', size=10)
  ) +
  guides(fill = guide_legend(title.position = 'bottom'))

fn <- str_c('sunset-time-oct31')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1180, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1180, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
source('upload-svg.R', local = T)
