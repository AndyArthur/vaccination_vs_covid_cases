library(tidyverse)
library(tigris)
library(sf)
library(ggtext)

infl <- read_csv('Desktop/inflation.csv') 

infl$DIVISION <- as.character(infl$DIVISION)

division <- states() %>% st_drop_geometry() %>% select(GEOID, DIVISION)

st <- states(cb=T) %>%   
  shift_geometry() %>% 
  filter(GEOID < 60)

st %>% 
  inner_join(division, by=c('GEOID')) %>%
  inner_join(infl, by=c('DIVISION')) %>%
  group_by(DIVISION) %>%
  summarize(inflation = first(inflation)) %>%
  ggplot() + 
  geom_sf(aes(fill=inflation)) +
  geom_sf(data=st, fill=NA) +
  geom_sf_label(aes(label=scales::percent(inflation, scale=1, accuracy = 0.1)))+
  scale_fill_distiller(palette = 'Oranges', direction = 1, labels=scales::percent_format(scale=1, accuracy = 0.1, breaks=seq(0,10,0.5))) +
  labs(title = 'Increase in the <span style="color: brown; font-size: 30pt">Consumer Price Index, Over Year Ago</span> in April 2022',
       y = "",
       x = "",
       caption='Bureau of Labor Statistics, Consumer Price Index',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=28,margin=unit(c(0,0,3,0),'pt')),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(0,0,3,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'top',
    legend.key.height = unit(0.3, 'cm'),
    legend.key.width = unit(5, 'cm'),
  ) +
  coord_sf(expand=F) 


width = 1920
bbox <- states() %>% shift_geometry() %>% st_transform(3857) %>% st_bbox()
height <- 1300 #((abs(bbox[4]-bbox[2])/abs(bbox[3]-bbox[1]))+0.05)#*1920
fn <- 'inflation'
  
ggsave(str_c('/tmp/',fn,'.jpg'), width=width, height=height, units='px', dpi=130)
ggsave(str_c('/tmp/',fn,'.svg'), width=width, height=height, units='px', dpi=130, device = grDevices::svg)
system(str_c('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz'))
