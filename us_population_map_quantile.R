library(tigris)
library(tidyverse)
library(classInt)
library(tidycensus)

pop2020 <- get_decennial(geography = "county", variables = "P1_001N", year = 2020, cache=TRUE)

county <- counties(cb=TRUE, resolution = '20m') %>% filter(STATEFP != 72) %>% shift_geometry() 
states <- states(cb=TRUE, resolution = '20m')  %>% filter(STATEFP != 72) %>% shift_geometry()

us <- sf::st_union(states)

joined <- left_join(county, pop2020, by=c("GEOID"="GEOID"))

classes <- classIntervals(joined$value/(joined$ALAND/2.59e+6), n = 15, style = "quantile")
  
joined <- joined %>% mutate(percent_class = cut(joined$value/(joined$ALAND/2.59e+6), breaks=classes$brks, include.lowest = T, 
                                                labels=paste(scales::comma(classes$brks[1:length(classes$brks)-1],1), scales::comma(classes$brks[2:length(classes$brks)],  accuracy=1 ), sep=' - ' ) ) )

ggplot(joined, aes(fill=percent_class)) + geom_sf(size=0.1) +
  geom_sf(data=states, mapping=aes(), fill=NA, color='gray') +
  ggfx::with_outer_glow(geom_sf(data=us, mapping=aes(), fill=NA, color='gray', size=0.2)) +
  scale_fill_viridis_d(option='D', 
                       direction = 1,
                       name = 'Residents\nPer Square Mile'
  ) +
  labs(title = "2020 County Population Density",
       caption='2020 PL 94-171 Data',
       tag='Andy Arthur, 12/10/2021',
  ) + 
  theme_void() + # dark theme
  theme(
    text= element_text(family='Overpass',size=14, color="black"),
    plot.title=element_text(hjust=0.5, face='bold',size=36),
    plot.background = element_rect(fill = "White", size=0),
    panel.background = element_rect(fill = "White", size=0),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='black'),
    plot.caption=element_text(size=10, color='black'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(3,'cm'),
    legend.position = 'bottom',
  )

ggsave('/home/andy/Desktop/County.svg', width=1920, height=1080, units='px', dpi=150, device = grDevices::svg)
