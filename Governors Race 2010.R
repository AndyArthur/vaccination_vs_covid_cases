library(tidyverse)
library(sf)
library(cowplot)
rm(list=ls())
tdir <- tempdir()

unzip('/home/andy/Downloads/tl_2012_36_vtd10.zip', exdir = tdir)

vt10 <- read_csv(archive::archive_read('/home/andy/Downloads/REDIST_DATA.zip', 'vtd10vote.csv'))

nys <- states(cb=T) %>% filter(STUSPS == 'NY') %>% st_transform(3857)

vtd10 <- read_sf(str_c(tdir,'/tl_2012_36_vtd10.shp')) %>% st_transform(3857) %>% st_intersection(nys)

vt10$NOPAD_VTDID <- as.character(vt10$NOPAD_VTDID)

vtd10 %>% inner_join(vt10, by=c('GEOID10'="NOPAD_VTDID")) -> vtd

nyco <- counties(cb=T, 'ny', resolution = '500k') %>% st_transform(3857) 

ggplot() + 
  geom_sf(data=vtd, aes(fill=DEM10G21/(DEM10G21+REP10G21)), size=0) + 
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
  labs(title = str_c('<span style="font-size: 48pt; color: gray10">2010 Governor</span> <span style="font-size: 48pt; color: blue">Andrew Cuomo</span> vs. <span style="font-size: 48pt; color: red">Carl Paladino</span>'),
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

fn <- str_c('cuomo-paladino')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg, bg='white')
#ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg, bg='white')
#system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
#unlink(str_c('~/Desktop/',fn,'.svg'))



