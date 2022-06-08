library(tidyverse)
library(tigris)
library(sf) 
library(zoo)
library(sf)
library(reticulate)

options(tigris_use_cache = TRUE)

ueRate <- read_csv('/tmp/current-ue.csv') %>% mutate(NAMELSAD10 = `AREA`)

ueArea <- read_sf('/home/andy/NYSDOL Unemployment Areas.geojson') %>% left_join(ueRate, by='NAMELSAD10')

ueArea %>% write_sf('/tmp/uerate.gpkg')

counties <- counties(state='ny', cb=TRUE)
cousub <- county_subdivisions(state='ny', cb=TRUE) %>% st_simplify(preserveTopology = TRUE, dTolerance = 1000)

countiesmerge <- st_sf(st_union(counties))

ueArea <- ueArea %>% st_transform('EPSG:4269') %>% st_intersection(counties) 

ueArea %>% write_sf('/tmp/uemap.gpkg')


ggplot(data = ueArea, aes(fill = ueArea$X2021.12.01)) + 
  geom_sf(size=0.01) +
  geom_sf(data=counties, size=0.3, fill=NA, col="#888888")+
  scale_fill_distiller(palette = "RdYlGn", 
                       direction = -1, breaks=seq(0,max(ueArea$X2021.12.01),1)) + 
  geom_sf(data=cousub, size=0.03, fill=NA)+
  ggfx::with_outer_glow(geom_sf(data=countiesmerge, fill=NA, size=0.3),col="gray",signma=10 )+
  coord_sf(crs='EPSG:3857') +
  labs(title = 'Unemployment Rate - December 2021',
#       subtitle='In October 2021, full employment returned to most of Upstate,\nhowever persistent-high unemployment, post-pandademic shutdowns, \nremains a problem in many cities including New York City.',
       y = "",
       x = "",
       caption='NYS Department of Labor',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'bottom',
  ) +
  guides(fill = guide_colourbar(barwidth = 50, barheight = 0.7,ticks = FALSE, size=0.3,
                                frame.colour = "#333333", label.position = "top"))


ggsave('/tmp/uerate.jpg', width=1920, height=1600, units='px', dpi=150)
ggsave('/tmp/uerate.svg', width=1920, height=1600, units='px', dpi=150, device = grDevices::svg)
