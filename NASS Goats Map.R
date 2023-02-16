library(usdarnass)
library(tigris)
library(tidyverse)
library(classInt)

rm(list=ls())
goats <- nass_data(
                    source_desc = 'CENSUS',
                     year = 2017,
                     short_desc = "GOATS - INVENTORY",
                     domain_desc = 'TOTAL'
                  )

goats$Value <- parse_number(goats$Value)

usco <- counties(cb=T, resolution = '20m') %>% shift_geometry() %>% filter(STATEFP < 58) %>% rmapshaper::ms_simplify()
usst <- states(cb=T, resolution = '20m')  %>% shift_geometry() %>% filter(STATEFP < 58)

usco <- left_join(usco, goats, 
                  join_by(STATEFP == state_fips_code,
                          COUNTYFP == county_code))

usco %>% transmute(NAMELSAD, STUSPS, 
                           PerSqMi = Value / st_area(.) %>% 
                     units::set_units('mi^2') %>% units::drop_units()) %>%
ggplot() +
  geom_sf(aes(fill=PerSqMi), linewidth=0.1) +
  geom_sf(data=usst, mapping=aes(), fill=NA, linewidth=0.4) +
  scale_fill_viridis_b(option='C', 
                       direction = 1,
                       limits=c(0,10), 
                       n.breaks=7,
                       name = 'Goats per square mile'
  ) +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt; color: lightyellow">Goats</span><br />',
                     '<span style="font-size: 26pt">2017 Agriculture Census</span>'),
       y = "",
       x = "",
       tag=paste('Data Source: National Agricultural Statistics Service.<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m.%-d.%y.")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14, color="gray90"),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=12, width=0.5, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "gray10", color="gray10"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.03),
    legend.position = c(0.8,1.04),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
  ) +
  guides(fill = guide_colourbar(title.position = "top", ticks = F))

fn <- str_c('goats')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=130)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

