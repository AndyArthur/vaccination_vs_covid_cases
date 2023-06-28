library(sf)
library(tidyverse)
library(terra)
rm(list=ls())

wms_url <- 'WMS:https://dmsdata.cr.usgs.gov/geoserver/wms?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&LAYERS=lcmap_primary-landcover_conus_year_data%3Aprimary-landcover_conus_year_data&FORMAT=image/tiff&SRS=EPSG:3857&BBOX=-1.439108301367112E7,2488126.723717102,-7026304.485240375,6968528.631338398'

bbx<- tigris::county_subdivisions('ny') %>% 
  filter(NAME == 'Albany') %>% 
  st_transform(3857) %>% 
  st_bbox()

yr <- 1986 
url <- wms_url %>%
  str_replace('SRS=(.*?)&', str_c('SRS=EPSG:3857&time=',yr,'-01-01T00:00:00.000Z&')) %>%
  str_replace('BBOX=(.*?)$', str_c('BBOX=',paste(bbx, collapse=',')) ) 

t <- tempfile()
reso <- c('-tr', '30','30','-co','COMPRESS=DEFLATE') # "meters" in the 3857 projection
sf::gdal_utils('translate', url, t, reso) 
r1985 <- rast(t, lyrs=1)
r1985 <- (r1985 == 255)

t2 <- tempfile()
yr <- 2021
url <- wms_url %>%
  str_replace('SRS=(.*?)&', str_c('SRS=EPSG:3857&time=',yr,'-01-01T00:00:00.000Z&')) %>%
  str_replace('BBOX=(.*?)$', str_c('BBOX=',paste(bbx, collapse=',')) ) 

reso <- c('-tr', '30','30','-co','COMPRESS=DEFLATE') # "meters" in the 3857 projection
sf::gdal_utils('translate', url, t2, reso) 
r2021 <- rast(t2, lyrs=1)
r2021 <- (r2021 == 255)

names(r2021) <- 'r'

nycos <- county_subdivisions('ny')

as.polygons((r2021-r1985) == 1) %>%
  st_as_sf() %>%
  filter(r == 1) %>%
  ggplot() + geom_sf(fill='orange', linewidth=0) +
  geom_sf(data=nycos, fill=NA) +
  coord_sf(crs = 3857, expand = F, xlim=c(bbx$xmin, bbx$xmax), ylim=c(bbx$ymin, bbx$ymax)) +
  theme_void() +
  labs(
    title = 'Areas Developed in Albany, 1986-2021',
    subtitle = 'Land Change Monitoring, Assessment, and Projection (LCMAP) comparing areas developed over a 35-year period in the City of Albany and nearby areas.',
    tag = paste("Andy Arthur,", format(Sys.Date(), format = "%-m/%-d/%y"), "")
  ) +
  theme(
    text = element_text(family = "Roboto Condensed", size = 18),
    plot.title = ggtext::element_textbox(size = 28, face = "bold", hjust = 0.5, halign = 0.5),
    plot.subtitle = ggtext::element_textbox(size = 14, width=0.7, hjust = 0.5, halign = 0.5, margin=margin(t=10,b=20)),
    plot.background = element_rect(fill = "#FFFCFF", color = "#FFFCFF"),
    plot.tag = ggtext::element_textbox(size = 12, hjust = 1, color = "#555555", halign = 1, valign = 0, margin=margin(t=10)),
    plot.tag.position = 'bottom',
    legend.position = "none"
  )

fn <- str_c("areas-developed")
ggsave(paste("/tmp/", fn, ".jpg", sep = ""), width = 1920, height = 1700, units = "px", dpi = 150)
ggsave(paste("/tmp/", fn, ".svg", sep = ""), width = 1920, height = 1700, units = "px", dpi = 150, device = grDevices::svg)
system(paste("scour /tmp/", fn, ".svg /tmp/", fn, ".svgz", sep = ""))

source('upload-svg.R', local=T)
