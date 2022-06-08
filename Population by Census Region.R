library(tidycensus)
library(tidyverse) 
library(wacolors)
library(ggtext)

pop2020 <- get_decennial(geography = "state", variables = "P1_001N",  year = 2020, geometry = T) %>% 
  shift_geometry()

regionpop <- pop2020 %>% 
  inner_join(data.frame(state.name, state.region), by=c('NAME'='state.name')) %>%
  group_by(state.region) %>%
  summarise(value = sum(value)) 

st <- states(cb=T, resolution = '20m') %>% 
  filter(STUSPS != 'PR') %>%
  shift_geometry()

regionpop <- regionpop %>% mutate(state.region = str_replace(state.region, 'North Central', 'Mid-West'))

regionpop %>%
  ggplot() + 
  geom_sf(aes(fill=value)) + 
  geom_sf(data=st, fill=NA, size=0.3) +
  ggsflabel::geom_sf_label_repel(aes(label=str_c(round((value/330759736)*100,0),'% ',state.region,'\n',scales::comma(value)), geometry=geometry),
                                 size=2.5, point.size = NA, fill='white', box.padding = 0.2, label.padding = unit(0.2, "lines"), family='Noto Sans', max.overlaps=NA ) +
  scale_fill_wa_c(palette = 'stuart', labels=scales::comma_format()) +
  labs(title = str_c('<span style="color: darkgreen">Population</span> by <span style="color: darkblue">Census Region</span>'),
       y = "",
       x = "",
       caption='2020 US Census',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=32, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'none',
  ) +
  coord_sf(expand=F)  

fn <- str_c('census-region')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

