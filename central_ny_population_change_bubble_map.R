library(tidycensus)
library(sf)
library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE)

pop2000 <- get_decennial(geography = "county subdivision", state='ny', variables = "P001001", year = 2000, cache=TRUE) 
pop2020 <- get_decennial(geography = "county subdivision", state='ny', variables = "P1_001N", year = 2020, cache=TRUE, geometry = TRUE)

county <- counties(state='ny', cb=TRUE)

popChg <- pop2020 %>% inner_join(pop2000, by = 'GEOID') %>% mutate('Change' = abs((value.y-value.x)/(value.x)), 'Growth' = value.x-value.y>0) %>% 
  separate(NAME.x, c('Municipality','County','State'), sep=", ")

popChg$Municipality <- str_replace(popChg$Municipality, ' ','\n')

bbox <- county %>% filter(NAME %in% c('Oswego','Madison')) %>% st_bbox()
  
ggplot(popChg) +
  geom_sf(mapping=aes(), data=county, fill=NA, size=0.6) +
  geom_sf(data=st_centroid(popChg), mapping = aes(size=Change, color=Growth)) +
  geom_sf_text(aes(label=paste(popChg$Municipality,scales::comma(value.x-value.y, accuracy = 1), sep='\n')), size=2.3, check_overlap=T) +
  geom_sf(aes(), fill=NA,alpha=0.8, size=0.3) +
  scale_color_discrete(name="Population",
                       labels=c("Loss", "Growth")) +
  scale_size_continuous(name="Change", labels = scales::percent_format(accuracy = 1), 
                        breaks=seq(0,max(popChg$Change, na.rm=TRUE), 0.05),  range=c(0,7), limits = c(0,.3) ) +
  labs(
    title='Central New York Population Change, 2000 to 2020',
    tag='Andy Arthur, 12/11/2021',
    caption='2000 and 2020 US Census'
  ) +
  theme_void()+
  coord_sf(xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4])) +
  theme(
    #legend.position = c(0.9, 0.7),
    text = element_text(color = "#22211d", family = 'Overpass'),
    plot.title = element_text(size= 26, hjust=0.5, color = "#4e4d47", face='bold', margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.tag.position = c(0.0,0.01),
  )
ggsave('Central NY_Change.svg', width=1920, height=1600, units='px', dpi=150, device = grDevices::svg )
ggsave('Central NY_Change.jpg', width=1920, height=1600, units='px', dpi=150, bg='white' )
