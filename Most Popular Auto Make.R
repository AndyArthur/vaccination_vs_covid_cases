library(data.table)
library(tigris)
library(sf)
library(tidyverse)
library(ggtext)

rm(list=ls())

auto <- fread(cmd='unzip -p autoreg.csv.zip')

colnames(auto)
mk <- auto[`Record Type`=='VEH' & State=='NY',.(.N),by=c('Zip','Make')] 

mk %>% hutils::mutate_other('Make', n=7, var.weight = 'N') %>% .[] -> ck

zips <- tigris::zctas(cb=T, year=2019)

ct <- counties('ny', cb=T)

ck %>% group_by(Zip) %>% 
  slice_max(N, n=1) %>%
  filter(Zip > 10000 & Zip < 20000) %>%
  mutate(Zip = as.character(Zip),
         Make = str_to_title(Make),
         Make = str_replace(Make, 'Other.', 'Other')
         ) %>% 
  inner_join(zips, by=c('Zip'='GEOID10')) %>% 
  st_as_sf()  %>%
  group_by(Make) %>% summarise() %>%
  ggplot() + 
  geom_sf(aes(fill=Make), size=0.1, color='white') +
  scale_fill_brewer(palette = 'Set1') + 
  geom_sf(data=ct, fill=NA, size=0.6, color='white') +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 38pt; color: navy">Most Popular Automobile Make</span><br /><br />Domestic manufacturers continue to dominate roads of much of Upstate NY.</em>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: NYS DMV Vehicle, Snowmobile, and Boat Registrations. \nhttps://data.ny.gov/Transportation/Vehicle-Snowmobile-and-Boat-Registrations/w4pv-hbkt'),
       fill = "")  +
  theme(
    legend.key.height = unit(1.5,'cm'),
    legend.key.width = unit(1,'cm'),
    legend.position = c(0.27,0.17),
    legend.spacing.y = unit(0.5, 'cm'),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(5,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) +
  guides(fill = guide_legend(byrow = TRUE, direction = 'horizontal') )

fn <- str_c('most-pop-auto-make')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))

