# example: https://andyarthur.org/svgz-graphic-change-in-county-jail-population-2010-to-2020.html

library(tidyverse)
library(readr)
library(stringr)
library(sf)

jail <- read_csv("https://data.ny.gov/api/views/nymx-kgkn/rows.csv?accessType=DOWNLOAD&sorting=true")

jailj <- jail %>% mutate(`Facility Name (ORI)` = str_replace(`Facility Name (ORI)`, 'County .*$', 'County')) %>% 
  select(`Facility Name (ORI)`,Year,Census) %>% pivot_wider(names_from = Year, values_from = Census, values_fn = sum) %>% 
  mutate(Chg = ((`2020`-`2010`)/`2010`)*100) %>% select(`Facility Name (ORI)`,`2020`,`2010`,Chg)

hex <- read_sf('Documents/GIS.Data/census.tiger/36_New_York/NY Counties Hexagram.gpkg')

hexj <- inner_join(hex, jailj, c('NAMELSAD10'='Facility Name (ORI)'))

ggplot(hexj, aes(fill=`Chg`)) + 
  ggfx::with_shadow(geom_sf(hex, mapping=aes(), fill='lightgray'), signma=0, x_offset=2, y_offset = 2) +
  geom_sf() + 
  geom_sf_label(aes(label=paste(`NAME10`,scales::percent(Chg, scale=1, accuracy=1), sep='\n')), fill='white',
                family='Open Sans', alpha=0.9, size=2.8, label.padding = unit(.1, 'lines')) +
  scale_fill_viridis_c(direction=-1,
                       breaks=seq(-150,200,10),
                       label = scales::percent_format(accuracy = 1, scale=1)
                       )+
  labs(title = paste("County Jail Population,\nChange Since 2010"),
       #subtitle='Western NY, Tug Hill and Mohawk Valley have high rates of COVID-19 as we head into the holiday weekend',
       y = "",
       x = "",
       caption='NYS Commission on Correction, data.ny.gov',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%-d/%y")),
       fill = "") +
  # While bail reform is responsible for drops in the past two years, this has been dropping for some time -- as crime rates are down and drug laws were reformed in late 2000s.
  theme_void() +
  theme(
    plot.title=element_text(hjust=0.5, family='Overpass', face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(family='Overpass',size=10,hjust=0, color='#555555'),
    plot.caption=element_text(family='Overpass', size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'bottom',
  ) +
  guides(fill = guide_colourbar(barwidth = 40, barheight = 0.7,ticks = FALSE, size=0.3,
                                frame.colour = "#333333", label.position = "top"))
  

ggsave(paste('/tmp/county-jail.jpg',sep=''), width=1920, height=1600, units='px', dpi=150)
ggsave(paste('/tmp/county-jail.svg',sep=''), width=1920, height=1600, units='px', dpi=150, device = grDevices::svg)
