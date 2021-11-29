library(readxl)
library(sf)
library(tidyverse)

X2021_albany_county_races <- read_excel("Desktop/2021_albany_county_races.xlsx", 
                                        sheet = "5 County Legis...OUNTY LEG 37") %>% mutate(DistNo = ED)

eds <- read_sf('/home/andy/Albany County October 2020 ED.shp')  %>% filter(Muni == 'Coeymans') %>% inner_join(X2021_albany_county_races, by='DistNo')
acroads <- read_sf('/home/andy/Documents/GIS.Data/census.tiger/36_New_York/tl_2020_36001_edges.shp') %>%  st_transform('EPSG:3857') %>% st_intersection(eds)
hamlet <- read_sf('/home/andy/Documents/GIS.Data/census.tiger/36_New_York/tl_2021_36_pointlm.shp') %>% filter(MTFCC == 'C3081') %>% st_transform('EPSG:3857') %>% st_intersection(eds)

ggplot(eds) +
  ggfx::with_outer_glow(geom_sf(aes(fill=((eds$`Kristen L. Geoghegan (DEM)`+ eds$`Kristen L. Geoghegan (WOR)`)/(eds$Ballot-eds$Blanks))),size=0.1,color='black'), sigma=1) +
  geom_sf_text(size=4, label=eds$DistNo) +
  geom_sf(acroads, mapping=aes(), size=0.2, alpha=0.6)+
  geom_sf_label(data=hamlet,mapping=aes(label=FULLNAME), size=3) +
  scale_fill_distiller(palette = "RdBu", 
                       direction = 1, labels=  ~paste0(.x*100, "%"), limits = c(.25,.75) ) + 
  labs(title='County Legislature District 37 Special',
       subtitle='Kristen L. Geoghegan (DEM/WF) vs Zachary S. Collins (REP/CON)',
       caption='Albany County Board of Elections',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%d/%y")) 
  ) +
  theme_void() +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'right',
  ) +
  guides(fill = guide_colourbar(barwidth = 0.7, barheight = 30,ticks = FALSE, size=0.3,
                                frame.colour = "#333333", label.position = "left", title = NULL ))

ggsave(paste('/home/andy/Desktop/coeymans-special.svg',sep=''), width=1920, height=1080, units='px', dpi=150, device = grDevices::svg)
ggsave(paste('/home/andy/Desktop/coeymans-special.jpg',sep=''), width=1920, height=1080, units='px', dpi=150)

