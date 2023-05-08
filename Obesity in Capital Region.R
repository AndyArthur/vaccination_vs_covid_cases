library(tigris)
library(tidycensus)
library(sf)
library(ggtext)
library(tidyverse)
library(raster)
library(gstat)
rm(list=ls())

ob <- read_csv('~/Downloads/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2022_release.csv')

act <- tracts('ny', county=c('Albany','Saratoga','Schenectady','Rensselaer'), year=2010, cb = T) %>%
  mutate(GEOID = str_c(STATE,COUNTY,TRACT))

nyco <- counties('ny', cb=T) %>% filter(NAME %in% c('Albany','Saratoga','Schenectady','Rensselaer'))
nycos <- county_subdivisions('ny', county=c('Albany','Saratoga','Schenectady','Rensselaer'), cb=T)

viridis::cividis(8)

ob %>% 
  filter(StateAbbr == 'NY', Measure == "Obesity among adults aged >=18 years") %>%
  left_join(act, ., join_by(GEOID == LocationName)) %>%
  ggplot() + geom_sf(aes(fill=Data_Value), linewidth=0) +
  scale_fill_viridis_b(option = 'cividis',  n.breaks=8, name='', labels=scales::label_percent(scale=1)) +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) + 
  geom_sf(data=nycos, fill=NA, color='white', linewidth=0.1) + 
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="color: #DBC761FF; font-size: 48pt">Adult Obesity</span> ',
                     '<br />in the Capital Region'),
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),
       '<br />2022 PLACES Local Data for Better Health (CDC) - Census Tracts')
       )  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple( face='bold', size=20, margin=margin(10, 0,30,0), halign=0.5, hjust=0.5),
    plot.background = element_rect(fill = "ivory", color="ivory"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0.5, color='#555555', halign = 0.5, valign = 1, margin=margin(15,5,5,5), width=1),
    plot.tag.position = 'bottom',
    legend.position = c(1, 0.58),
    legend.justification = c(1,0),
    legend.key.height = unit(2,'cm'),
    legend.key.width = unit(1.3,'cm'),
    legend.text = element_text(margin = margin(t = 30, unit = "pt")),
  ) 

fn <- str_c('adult-obesity')
width <- 920
height <- width*4/3

ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=width, height=height, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''),  width=width, height=height, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


