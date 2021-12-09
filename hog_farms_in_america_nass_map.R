library(usdarnass)
library(tigris)
library(tidyverse)
library(classInt)

hogs <- nass_data(
                    source_desc = 'CENSUS',
                     year = 2017,
                     short_desc = "HOGS - INVENTORY",
                     domain_desc = 'TOTAL'
                     )

hogs$Value <- as.numeric(gsub(",", "",hogs$Value))


county <- counties(cb=TRUE, resolution = '20m') %>% filter(STATEFP != 72) %>% shift_geometry() 
states <- states(cb=TRUE, resolution = '20m')  %>% filter(STATEFP != 72) %>% shift_geometry()

us <- sf::st_union(states)

joined <- left_join(county, hogs, by=c("STATEFP"="state_fips_code", "COUNTYFP"="county_code"))

classes <- classIntervals(joined$Value/(joined$ALAND/2.59e+6), n = 9, style = "fisher", dataPercision=0)
  
joined <- joined %>% mutate(percent_class = cut(joined$Value/(joined$ALAND/2.59e+6), breaks=classes$brks, include.lowest = T, 
                                                labels=scales::comma(classes$brks[2:length(classes$brks)]),0) )

ggplot(joined, aes(fill=percent_class)) + geom_sf(size=0.1) +
  geom_sf(data=states, mapping=aes(), fill=NA, color='gray') +
  ggfx::with_outer_glow(geom_sf(data=us, mapping=aes(), fill=NA, color='gray', size=0.2)) +
  scale_fill_viridis_d(option='D', 
                       direction = 1,
                       name = 'Hogs Raised\nPer Square Mile'
  ) +
  labs(title = "2017 Hogs Per Square Mile",
       caption='2017 US Census of Agriculture, quickstats.nass.usda.gov',
       tag='Andy Arthur, 12/8/2021',
  ) + 
  theme_void() + # dark theme
  theme(
    text= element_text(family='Overpass',size=14, color="black"),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "MistyRose", size=0),
    panel.background = element_rect(fill = "MistyRose", size=0),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='black'),
    plot.caption=element_text(size=10, color='black'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(3,'cm'),
    legend.position = 'bottom',
  )

ggsave('/home/andy/Desktop/hog.jpg', width=1920, height=1400, units='px', dpi=150)
ggsave('/home/andy/Desktop/hog.svg', width=1920, height=1080, units='px', dpi=150, device = grDevices::svg)
