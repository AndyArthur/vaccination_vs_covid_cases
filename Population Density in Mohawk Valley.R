library(tigris)
library(tidycensus)
library(sf)
library(ggtext)
library(tidyverse)
library(raster)
library(gstat)
rm(list=ls())

county.list <- c('Madison','Oneida','Herkimer','Fulton','Montgomery','Schenectady')

acbk <- get_decennial('block', state='ny',  county=county.list, 
                      year=2020, variables = "P1_001N", geometry = T)

acbk <- acbk %>% mutate(density = value/(st_area(.) %>% units::set_units('mi^2')) %>% units::drop_units())

acbk <- acbk %>% rmapshaper::ms_simplify(0.01)

nyco <- counties('ny', cb=T) %>% filter(NAME %in% county.list)
nycos <- county_subdivisions('ny', county=county.list, cb=T)

viridis::inferno(8)

acbk %>%
  ggplot() + geom_sf(aes(fill=density), linewidth=0) +
  scale_fill_viridis_b(option = 'inferno',  n.breaks=8, name='Residents\nSq. Mile', labels=scales::comma_format(), trans='log10', limits=c(0.1,1e5), na.value = 'black') +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) + 
  geom_sf(data=nycos, fill=NA, color='white', linewidth=0.1) + 
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="color: #280B54FF; font-size: 48pt">Population Density</span> ',
                     '<br />in the Mohawk Valley'),
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),
       '<br />2020 US Census - Block Level')
       )  +
  theme(
    text= element_text(family='Chivo',size=14),
    plot.title=ggtext::element_textbox_simple( face='bold', size=20, margin=margin(10, 0,30,0), halign=0.5, hjust=0.5),
    plot.background = element_rect(fill = "snow", color="snow"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0.5, color='#555555', halign = 0.5, valign = 1, margin=margin(15,5,5,5), width=1),
    plot.tag.position = 'bottom',
    legend.position = c(0.99, 0.58),
    legend.justification = c(1,0),
    legend.key.height = unit(1.3,'cm'),
    legend.key.width = unit(1.3,'cm'),
    legend.text = element_text(margin = margin(t = 30, unit = "pt")),
  ) 

fn <- str_c('mohawk-valley-population-density')
width <- 1920
height <- width*0.9

ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=width, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',fn,'.svg',sep=''),  width=width, height=height, units='px', dpi=150, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

source('upload-svg.R', local=T)

