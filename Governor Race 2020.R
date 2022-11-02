library(tidyverse)
library(tigris)
library(ggtext)
library(sf)
library(cowplot)

rm(list=ls())

# load latfor data, VTD level data is okay for larger districts
# although latfor also provides block level data which is better as
# those centroids should always be inside the district you are comparing
vt20 <- read_csv('~/2020vote_vtd.csv')
vt20$GEOID <- as.character(vt20$GEOID)


# join the VTD data to voting districts
vtd <- voting_districts('ny', cb=T) %>%
  inner_join(vt20, by=c('GEOID20'='GEOID')) %>%
  st_transform('epsg:3857') #%>% rmapshaper::ms_simplify()

#urb <- urban_areas()
nys <- states(cb=T) %>% filter(STUSPS=='NY')
#uny <- urb %>% filter(st_intersects(.,nys,sparse=FALSE)[,1]) %>% 
#  st_transform('epsg:3857') %>% group_by() %>% summarise()

#uny <- uny %>% group_by() %>% summarise()
#vtduny <- vtd %>% filter(st_intersects(.,uny,sparse=FALSE)[,1])

nyco <- counties(cb=T, 'ny', resolution = '500k') %>% st_transform('epsg:3857') 

ggplot() + 
  geom_sf(data=vtd, aes(fill=DEM18G21/(DEM18G21+REP18G21)), size=0) + 
  scale_fill_steps2(midpoint = 0.5, low='red', high='blue', na.value = 'white', 
                    breaks=seq(0.1,1,.1), labels=scales::label_percent()) +
  

  geom_sf(data=nyco, fill=NA, color='white') +
  theme_void() -> main

# NYC Subset
bbox <- nyco %>% filter(NAME %in% c('Richmond','Bronx','Queens')) %>% st_bbox()
nyc <- main + coord_sf(xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]), expand=T, crs=3857) +
  theme(panel.border = element_rect(fill=NA, size=0.5), legend.position = 'none',
        plot.title=element_text(family='Lato',size=12, margin=margin(b=3))) +
  labs(title='New York City')  

nys <- main + 
  coord_sf(expand=F, crs=3857) +
  labs(title = str_c('<span style="font-size: 48pt; color: gray10">2018 Governor</span> <span style="font-size: 48pt; color: blue">Andrew Cuomo</span> vs. <span style="font-size: 48pt; color: red">Marc Molinaro</span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: LATFOR'),
       fill = "")  +
  theme(
    legend.key.height = unit(1.5,'cm'),
    legend.key.width = unit(1,'cm'),
    legend.position = c(0.19,0.17),
    legend.spacing.y = unit(0.5, 'cm'),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(25,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) +
  guides(fill = guide_legend(byrow = TRUE, direction = 'horizontal') )


ggdraw() +
  draw_plot(nys) +
  draw_plot(nyc, x = 0.3, y = 0.04, width = 0.35, height = 0.26)



fn <- str_c('cuomo-molinaro')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg, bg='white')
#ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg, bg='white')
#system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))


 
