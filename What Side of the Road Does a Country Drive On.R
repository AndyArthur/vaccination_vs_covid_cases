library(tidyverse)
library(rvest)

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

lhd <- read_html('https://www.worldstandards.eu/cars/list-of-left-driving-countries/')

lhd <- lhd %>% html_table() %>% .[[1]] %>%
  mutate(`Country / state / territory` = str_replace(`Country / state / territory`,'\\(.*\\)','') %>%  str_trim() ) 

countries <- ne_countries(scale='small') %>% st_as_sf() %>%
  left_join(lhd, by=c(name_sort='Country / state / territory')) %>%
  mutate(`left / right` = ifelse(is.na(`left / right`) & sovereignt != 'Antarctica', 'right', `left / right`),
         `left / right` = ifelse(sovereignt == 'eSwatini', 'left', `left / right`)
         )

ocean <- ne_load(type='ocean', category = 'physical', scale='small', destdir = '~/.cache/RStudio/') %>% st_as_sf()

ggplot() + 
  geom_sf(data=ocean, fill='gray80',size=0) +
  geom_sf(data=countries, aes(fill=`left / right`), size=0.2) + 
  scale_fill_brewer(palette = 'Greens') +
  coord_sf(crs = '+proj=natearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs', expand=F) +
  theme_void() +
  labs(title='Earth Map') +
  labs(title = str_c('<span style="font-size: 40pt; color: lightgreen">What side of the road does a country drive on?</span> '),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"), '<br />Source: worldstandards.eu/cars/list-of-left-driving-countries/'),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.01),
    legend.position = c(1,1.02),
    legend.justification = 'right',
    legend.direction = 'horizontal',
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.box = 'horizontal',
    
    #legend.text = element_text(margin = margin(t = 30, unit = "pt")),
  )  +
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

fn <- str_c('road-drive')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1080, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))



