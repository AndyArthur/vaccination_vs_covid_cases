library(tidycensus)
library(sf)
library(tigris)
library(tidyverse)

pop2020 <- get_decennial(geography = "county", variables = "P1_001N", 
                         year = 2020, cache=TRUE, geometry = F)

mtbg <- counties() %>% inner_join(pop2020, by=c('GEOID'='GEOID')) %>%
  separate(NAME.y, c('County','State'), ', ') %>% 
  mutate(Population_per_Sq_Mi = value/(ALAND/2589988.1103)  )  %>%
  group_by(STATEFP) %>% slice_min(Population_per_Sq_Mi, n=1) %>%
  filter(STATEFP < 60 & State != 'Alaska' & State != 'Hawaii') #%>%
  #write_sf('/tmp/leastdense.gpkg') 
  #st_centroid(.)

states <- states(cb=T) %>% filter(STATEFP < 60 & NAME != 'Alaska' & NAME != 'Hawaii') 

ggplot(mtbg) + geom_sf(data=states, fill='beige', size=0.3, alpha=0.8) + 
  geom_sf(color=white, fill='darkgreen') +
  ggsflabel::geom_sf_label_repel(aes(label=paste(County,'\n',round(Population_per_Sq_Mi,1),'ppl sq mi')), size=2.5,
                                 box.padding = 0.5, point.size = 6) +
  theme_void() +
  coord_sf(expand = F, crs='EPSG: 5070') +
  labs(title = 'Lowest Population Density Counties in Each State',
       x='',
       y='',
       caption='2020 US Census',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=26),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5, face = 'italic'),
    plot.tag=element_text(size=10,hjust=0),
    axis.title.y.left = element_text(angle=90, face='italic'),
    axis.title.y.right = element_text(angle=-90, face='bold',size=36),
    plot.caption=element_text(size=10),
    legend.margin = margin(10,0,0,0),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'None'
  )

filename <- 'low_pop_denisty'
height <- 1300
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=150, device = grDevices::svg)
