library(tidyverse)
library(tigris)
library(rvest)
library(ggtext)

iihs <- read_html('https://www.iihs.org/topics/fatality-statistics/detail/state-by-state')

usst <- states(cb=T, resolution = '20m') %>% shift_geometry() %>% filter(STUSPS != 'PR') %>% rmapshaper::ms_simplify()

iihs %>%
  html_table() %>% 
  .[[1]] %>% 
  janitor::row_to_names(1) %>%
  left_join(usst, ., by=c('NAME'='State')) %>%
  mutate(across(10:14, ~parse_number(.))) %>%
  ggplot() + geom_sf(aes(fill=`Vehicle miles traveled (millions)`*1e6/Population), color='white', linewidth=1) +
  scale_fill_viridis_b(labels=scales::label_comma()) + 
  theme_void() +
coord_sf(expand = F) +
  labs(
    title = str_c('<span style="color: #440154FF; font-size: 35pt;">Vehicle Miles Traveled Per Capita, 2020.</span>'),
    y = "",
    x = "",
    caption = "",
    tag = paste("Andy Arthur,", format(Sys.Date(), format = "%-m/%-d/%y"), "<br /><em>Source:</em> Insurance Institute for Highway Safety. iihs.org/topics/fatality-statistics/detail/state-by-state"),
    fill = ""
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto Condensed", size = 14),
    plot.title = element_textbox(hjust = 0, face = "bold", size = 28, margin = margin(0, 0, 10, 0)),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.tag = ggtext::element_textbox(size = 10, hjust = 1, color = "#555555", maxheight = 0, halign = 1),
    plot.caption = element_text(size = 10, color = "#555555"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.tag.position = c(1, 0.03),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(2.2, "cm"),
    legend.direction = "horizontal",
    legend.position = c(1, 1.04),
    legend.justification = "right",
  )

fn <- str_c('vehicle-miles-per-capita')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))

