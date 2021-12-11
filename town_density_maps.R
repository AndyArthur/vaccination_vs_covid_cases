library(tigris)
library(tidyverse)
library(classInt)
library(tidycensus)
library(sf)

blocks <- get_decennial(geography = "block", state='ny', county='albany', variables = "P1_001N", 
                         year = 2020, cache=TRUE, geometry = TRUE) %>% st_transform('EPSG:3857')

countysub <- county_subdivisions(state='ny', county='Albany') %>% st_transform('EPSG:3857')

acroads <- roads(state='ny',county='albany') %>% st_transform('EPSG:3857') 

for (town in countysub$NAMELSAD) {
  countysubpart <- countysub %>% filter(NAMELSAD == town)
  blockpart <- st_intersection(countysubpart, blocks)
  roadpart <- st_intersection(countysubpart, acroads)
  
  classes <- classIntervals(blockpart$value/(blockpart$ALAND10/2.59e+6), n = 17, style = "fisher")
    
  blockpart <- joined %>% mutate(percent_class = cut(blockpart$value/(blockpart$ALAND10/2.59e+6), breaks=classes$brks, include.lowest = T, 
                                                  labels=paste(scales::comma(classes$brks[1:length(classes$brks)-1],1), scales::comma(classes$brks[2:length(classes$brks)],  accuracy=1 ), sep=' - ' ) ) )
  
  ggplot(blockpart, aes(fill=percent_class, geometry=geometry)) + geom_sf(size=0.01) +
    geom_sf(data=roadpart, mapping=aes(), size=0.3, fill=NA) +
    scale_fill_viridis_d(option='C', 
                         direction = 1,
                         name = 'Residents\nPer Square Mile',
                         na.value = 'gray'
    ) +
    labs(title = paste("2020",str_to_title(town),"Population Density"),
         caption='Andy Arthur, 12/11/2021                                                                                                                                                                  2020 PL 94-171 Data',
    ) + 
    theme_void() + # dark theme
    theme(
      text= element_text(family='Overpass',size=12, color="black"),
      plot.title=element_text(hjust=0.5, face='bold',size=26),
      plot.subtitle=element_text(hjust=0.5),
      plot.tag=element_text(size=10,hjust=0, color='black'),
      plot.caption=element_text(size=10, hjust = 0.5, color='black'),
      plot.margin = unit(c(1,1,1,1), 'lines'),
      plot.tag.position = c(0.0,0.01),
      legend.key = element_rect(size=3, color='white'),
      legend.text = element_text(family='Overpass Mono',size=8, color="black"),
      legend.position = 'right',
      legend.margin = margin(b=10),
    )
  
  ggsave(paste('/home/andy/Desktop/county-svg/',town,'.svg',sep=''), width=1920, height=1080, units='px', dpi=150, device = grDevices::svg)
}
