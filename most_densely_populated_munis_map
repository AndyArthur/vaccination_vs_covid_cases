library(tigris)
library(tidyverse)
library(classInt)
library(tidycensus)

pop2020 <- get_decennial(geography = "county subdivision", state='ny', variables = "P1_001N", year = 2020, cache=TRUE)
county <- county_subdivisions(state='ny', cb=TRUE)

joined <- left_join(county, pop2020, by=c("GEOID"="GEOID"))

joined$density <- joined$value/(joined$ALAND/2.59e+6)

states <- states(cb=T) %>%  
  filter(`STUSPS` == 'NY')

dense <- joined %>% slice_max(density,n=25)

ggplot(dense) + geom_sf(data=states, fill='beige', size=0.3, alpha=0.8) + 
  geom_sf(fill='red',color='black',size=0.2) +
  ggsflabel::geom_sf_label_repel(aes(label=NAME.x), size=2.5,
                                 box.padding = 0.5, max.overlaps = Inf) +
  theme_void() +
  labs(title = '25 Most Densely Populated Municipalities in New York State',
       x='',
       y='',
       caption='2020 US Census',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  coord_sf(expand=F) +
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

filename <- '25_most_dense'
height <- 1300
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=150, device = grDevices::svg)

