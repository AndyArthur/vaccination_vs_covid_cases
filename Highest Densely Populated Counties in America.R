library(tidycensus)
library(sf)
library(tigris)
library(tidyverse)

pop2020 <- get_decennial(geography = "county", variables = "P1_001N", 
                         year = 2020, cache=TRUE, geometry = F)

mtbg <- counties(cb=T) %>% inner_join(pop2020, by=c('GEOID'='GEOID')) %>%
  separate(NAME.y, c('County','State'), ', ') %>% 
  mutate(Population_per_Sq_Mi = value/(ALAND/2589988.1103)  )  %>%
  group_by(STATEFP) %>% slice_max(Population_per_Sq_Mi, n=1) %>%
  filter(STATEFP < 60 & State != 'Alaska' & State != 'Hawaii') #%>%
  #write_sf('/tmp/leastdense.gpkg') 
  #st_centroid(.)

states <- states(cb=T) %>% filter(STATEFP < 60 & NAME != 'Alaska' & NAME != 'Hawaii') 

ggplot(mtbg) + geom_sf(data=states, fill='gray93', size=0.3) + 
  geom_sf(color='black', fill='red', size=0.1) +
  ggsflabel::geom_sf_label_repel(aes(label=paste(County,'\n',scales::comma(Population_per_Sq_Mi,accuracy=1),'ppl sq mi')), size=2.5,
                                 box.padding = 0.5, point.size = 4) +
  theme_void() +
  coord_sf(expand = F, crs='EPSG: 5070') +
  labs(title = 'Highest Population Density Counties in Each State',
       x='',
       y='',
       caption='2020 US Census',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=26),
    plot.background = element_rect(fill = "Grey66", color=NA),
    plot.margin=unit(c(5,5,5,5), 'pt'),
    plot.subtitle=element_text(hjust=0.5, face = 'italic'),
    plot.tag=element_text(size=10,hjust=0),
    axis.title.y.left = element_text(angle=90, face='italic'),
    axis.title.y.right = element_text(angle=-90, face='bold',size=36),
    plot.caption=element_text(size=10),
    legend.margin = margin(10,0,0,0),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'None'
  )

filename <- 'Highest_Population_Density_Counties_in_Each_State'
height <- 1300
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=150, device = grDevices::svg)

system(paste('scour /tmp/',filename,'.svg', ' /tmp/',filename,'.svgz',sep=''))
