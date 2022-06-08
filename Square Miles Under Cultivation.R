library(tidycensus)
library(tidyverse)
library(terra)
library(FedData)
library(sf)
library(tigris)
library(ggtext)
library(units)
rm(list=ls())

hist <- read_csv('/tmp/cohist.csv') %>%
  select(GEOID10, starts_with('HIST'))

pxSize <- 30*30 %>% units::set_units(m^2) %>% units::set_units(mi^2)

nyco <- counties('ny',cb=T)

hist %>% 
  mutate(across(starts_with('HIST'), ~ .*pxSize),
         GEOID10 = as.character(GEOID10)) %>%
  inner_join(nyco, ., by=c('GEOID'='GEOID10')) %>%
  drop_units() %>%
  ggplot() +
  geom_sf(aes(fill=HISTO_82)) +
  scale_fill_gradient(high='#503715', low='white', label=scales::comma_format()) +
  ggsflabel::geom_sf_label_repel(aes(label=str_c(scales::comma(HISTO_82, accuracy = 1),''), geometry=geometry), segment.color='gray',
                                 size=2.5, point.size = NA, fill='white', box.padding = 0.2, label.padding = unit(0.2, "lines"), family='Noto Sans', max.overlaps=NA ) +
  labs(title = str_c('Square Miles of <span style="color: #503715">Cultivated Crops</span>' ),
      # subtitle = 'Some counties have reduced taxes more then others, but generally the tax holiday has modest impacts on cost of fueling up when 15 gallons costs $75.',
       y = "",
       x = "",
       caption='2019 NLCD',
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

fn <- str_c('sq-mi-culv')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

