library(tidycensus)
library(scales)
library(tidyverse)
library(sf)
library(tigris)
library(geofacet)

census_api_key('2e5efe5ba21d5ac8e0dd34ccd85db4a364cef118', install=TRUE, overwrite=TRUE)
options(tigris_use_cache = TRUE)

nyincome <- get_acs(
  geography = "county subdivision",
  state='ny',
  variables = c(medinc = "B19013_001"),
  year = 2019,
  geometry = T
)

poor <- nyincome %>% drop_na() %>%
  filter(!grepl('Rico', NAME)) %>% 
  slice_max(estimate, n=25) %>%
  separate(NAME, c('Muni','County','State'), ',') %>%
  st_centroid(.)

states <- states(cb=T) %>% shift_geometry() %>% 
  filter(`STUSPS` == 'NY')
  #filter(!STUSPS %in% c('PR','HI','AK'))

ggplot(poor) + geom_sf(data=states, fill='beige', size=0.3, alpha=0.8) + 
  geom_sf(color='darkgreen') +
  ggsflabel::geom_sf_label_repel(aes(label=Muni), size=2.5,
                                 box.padding = 0.5, max.overlaps = Inf) +
  theme_void() +
  labs(title = '25 Wealthiest Municpalities in New York State',
       x='',
       y='',
       caption='2019 ACS 5-Yr, Median Income',
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

filename <- '25_poor_wealthy'
height <- 1300
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=150, device = grDevices::svg)

