library(rvest)
library(tidyverse)
library(tigris)
library(zoo)
library(classInt)
library(janitor)
library(units)
library(ggtext)
library(lubridate)

rm(list = ls())
url <- "https://gasprices.aaa.com/state-gas-price-averages/"

states <- states(cb=T, resolution='20m') %>% 
  shift_geometry() %>% rmapshaper::ms_simplify(0.2)

gas <- read_html(url)

gas <- gas %>% html_table()
gas <- gas[[1]]

gas <- gas %>% mutate(across(2:ncol(.), parse_number))

cls <- colorspace::sequential_hcl(palette = 'Heat', n=6)

states %>% 
  inner_join(gas, by=c('NAME'='State')) %>%
  ggplot() +
  geom_sf(aes(fill=Regular), size=0.7, color='white') + 
  ggsflabel::geom_sf_text_repel(aes(label=str_c(STUSPS,'\n',scales::dollar(Regular, accuracy = 0.01)), geometry=geometry),
                                point.size = NA, force=0.1, family='Roboto Condensed', fontface='bold', max.overlaps=NA,
                                bg.color='#ffffff66', bg.r=0.2, size=4, box.padding=0.3
                                ) +
  scale_fill_stepsn(colors = cls, label=scales::dollar_format(), n.breaks=8) +
  coord_sf(expand=F)+
  theme_void() +
  labs(title = str_c('<span style="color: ',cls[3],'; font-size: 50px">Regular Gas Price</span> ',  format( now(),  format="%-B %-d, %Y") ),
       y = "",
       x = "",
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '<br />Data Source: gasprices.aaa.com/state-gas-price-averages' ),
       fill = "") +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, size=20, width = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12, hjust=0, color='#555555', maxheight=0, height=0, halign = 1, valign=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.76,0.03),
    strip.text = element_text(face='bold'),
    legend.key.height = unit(0.7,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(0.8,1.03)
  ) 

fn <- str_c('aa-gas-prices')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


cls <- colorspace::sequential_hcl(palette = 'ag_GrnYl', n=6)

states %>% 
  inner_join(gas, by=c('NAME'='State')) %>%
  ggplot() +
  geom_sf(aes(fill=Diesel), size=0.7, color='white') + 
  ggsflabel::geom_sf_text_repel(aes(label=str_c(STUSPS,'\n',scales::dollar(Diesel, accuracy = 0.01)), geometry=geometry),
                                  point.size = NA, force=0.3, family='Roboto Condensed', fontface='bold', max.overlaps=NA,
                                 bg.color='#ffffff66', bg.r=0.2, size=3.5
   ) +
  scale_fill_stepsn(colors = cls, label=scales::dollar_format(), n.breaks=8) +
  coord_sf(expand=F)+
  theme_void() +
  labs(title = str_c('<span style="color: ',cls[3],'; font-size: 50px">Diesel Price</span> ',  format( now(),  format="%-B %-d, %Y") ),
       y = "",
       x = "",
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '<br />Data Source: gasprices.aaa.com/state-gas-price-averages' ),
       fill = "") +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, size=20, width = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12, hjust=0, color='#555555', maxheight=0, height=0, halign = 1, valign=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.76,0.03),
    strip.text = element_text(face='bold'),
    legend.key.height = unit(0.7,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(0.8,1.03)
  ) +
  guides(size='none' )

fn <- str_c('aa-diesel-prices')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

