library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(classInt)
rm=ls()

stinfo <- states(cb=T) %>% filter(GEOID < 57) %>% arrange(NAME) %>% st_drop_geometry()

for (i in seq(1,length(stinfo$STUSPS))) {
  stflow <- get_flows(
    geography = "county",
    state = stinfo$STUSPS[i],
    year = 2019,
    geometry = F
  )
  
  stflow$ST2 <- substr(stflow$GEOID2,0,2)
  
  stflow <- stflow %>% filter(ST2 != stinfo$GEOID[i], variable=='MOVEDOUT') %>%
    group_by(ST2) %>% summarise(estimate = sum(estimate))
  
  classes <- classIntervals(stflow$estimate, n = 11, style = "fisher", dataPercision=0)
  
  stflow$est_class = cut(stflow$estimate, breaks=classes$brks, include.lowest = T, 
                       labels=scales::comma(classes$brks[2:length(classes$brks)], accuracy = 100),0) 
  
  st <- states(cb=T, resolution = '20m') %>% filter(GEOID < 57) %>% left_join(stflow, by=c('GEOID'='ST2')) %>%
    shift_geometry()
  
  ggplot(st) + geom_sf(aes(fill=est_class)) +
    ggsflabel::geom_sf_label_repel(aes(label=scales::comma(estimate, accuracy = 1)),size=3, point.size = NA, fill='white', label.padding = unit(0.2, "lines"),
                                   min.segment.length =0, box.padding = 0.3, segment.color='#999999', segment.size=0.8 )+ 
    scale_fill_viridis_d(name='Estimate') +
    theme_void() +
    labs(title = str_c(stinfo$NAME[i],' Residents Moved To These States'),
         caption='2019 US Census Population Flows',
         tag='Andy Arthur, 4/27/2022',
    ) + 
    coord_sf(expand=F) +
    theme_void() + # dark theme
    theme(
      text= element_text(family='Overpass',size=14, color="white"),
      plot.title=element_text(hjust=0.5, face='bold',size=36),
      plot.background = element_rect(fill = "black", size=0),
      panel.background = element_rect(fill = "black", size=0),
      plot.subtitle=element_text(hjust=0.5),
      plot.tag=element_text(size=10,hjust=0, color='white'),
      plot.caption=element_text(size=10, color='white'),
      plot.margin = unit(c(1,1,1,1), 'lines'),
      plot.tag.position = c(0.0,0.01),
      legend.key.width = unit(3,'cm'),
      legend.position = 'bottom',
    )
  
  filename <- str_c(stinfo$STUSPS[i], 'movedto')
  height <- 1250
  ggsave(paste('Desktop/stflow/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=120)
  #ggsave(paste('Desktop/stflow/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=120, device = grDevices::svg)
}
