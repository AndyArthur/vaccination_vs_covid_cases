library(rvest)
library(tidyverse)
library(tigris)
library(zoo)
library(classInt)
library(janitor)
library(ggtext)
library(lubridate)

rm(list=ls())
url <- 'https://gasprices.aaa.com/state-gas-price-averages/'

states <- states(cb=T) %>% shift_geometry()
gas <- read_html(url)

gas <- gas %>% html_table() 
gas <- gas[[1]]

gas <- gas %>% mutate(across(2:ncol(.), parse_number))

gas$Above5 <- gas$Regular >= 5 

states %>% 
  inner_join(gas, by=c('NAME'='State')) %>%
  ggplot() +
  geom_sf(aes(fill=Above5), size=0.3, color='black') + 
  ggsflabel::geom_sf_label_repel(aes(label=str_c(STUSPS,'\n',scales::dollar(Regular, accuracy = 0.01)), geometry=geometry),
                                 size=2.5, point.size = NA, fill='white', box.padding = 0.2, label.padding = unit(0.2, "lines"), family='Noto Sans', max.overlaps=NA ) +
  scale_fill_manual(values=c('darkblue','#cccc00'), labels=c('Below $5', '$5 or Above') ) +
  labs(title = str_c('<span style="color: darkblue">Gas Prices</span> <span style="color: #cccc00">Above Five Dollars</span> - ',  format( now(),  format="%-B %-d, %Y") ),
       subtitle = str_c('There are currently ',sum(gas$Above5),' states with gas prices above 5 dollars.'),
       y = "",
       x = "",
       caption='https://gasprices.aaa.com/state-gas-price-averages/',
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
    legend.position = 'top',
  ) +
  coord_sf(expand=F)  

fn <- str_c('aa-gas-prices')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
