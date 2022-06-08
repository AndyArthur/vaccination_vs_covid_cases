library(tidycensus)
library(sf)
library(tigris)
library(tidyverse)
library(cartogram)

pop2020 <- get_decennial(geography = "tract", state='tx', variables = "P1_001N", 
                         year = 2020, cache=TRUE, geometry = T)

pop2020$sqmi = pop2020$value/(st_area() %>% units::set_units(m^2) %>% units::drop_units())

#pop2020 %>% st_transform(3857) -> pop2020

#copop <- cartogram_ncont(pop2020, 'value')

nyco <- counties(cb=T, 'tx')

ggplot(pop2020) +
  geom_sf(aes(fill=value), size=0) +
  geom_sf(data=nyco, size=0.1, fill=NA) +
  theme_void() +
  scale_fill_viridis_c( label=scales::comma_format()) +
  labs(title = str_c('<span style="color: red">Texas</span> County Population'),
       y = "",
       x = "",
       caption='2020 US Census',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=30, margin=unit(c(5,0,3,0),'pt'), lineheight = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, face='italic', margin=unit(c(0,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'top',
  ) +
  coord_sf(expand=F)  

fn <- str_c('texas-pop')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


