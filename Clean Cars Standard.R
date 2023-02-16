library(tidyverse)
library(tigris)
library(rvest)
library(ggtext)

usst <- states(cb=T, resolution = '20m') %>% shift_geometry() %>% filter(STUSPS != 'PR') %>% rmapshaper::ms_simplify()

cleanCar <- c('CA','NY','WA','VT','MA','OR')
considerCleanCar <- c('RI','CT','ME','CO','MN','NJ','DE','VA','NV','PA','NM','MD')

usst %>%
  mutate(cleanCar = ifelse(STUSPS %in% cleanCar, '2', '0'),
         cleanCar = ifelse(STUSPS %in% considerCleanCar, '1', cleanCar),
         ) %>%
  ggplot() + geom_sf(aes(fill=cleanCar), color='white', linewidth=1) +
  scale_fill_manual(values=c('gray80','yellow','darkgreen')) + 
  theme_void() +
coord_sf(expand = F) +
  labs(
    title = str_c('<span style="color: darkgreen; font-size: 35pt; font-weight: bold">Clean Car Standard States</span><br />
                  <em>States in green have adopted regulations that call for all new cars by 2035 to either be plug-in hybrid or fully electric, while yellow states are actively considering such regulations.'),
    y = "",
    x = "",
    caption = "",
    tag = paste("Andy Arthur,", format(Sys.Date(), format = "%-m/%-d/%y"), "<br /><em>Source:</em> Bloomberg News, California ARB. news.bloomberglaw.com/environment-and-energy/states-adopt-california-car-rules-amid-national-standards-debate"),
    fill = ""
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto Condensed", size = 14),
    plot.title = element_textbox(halign = 0.5, size = 18, margin = margin(0, 0, 10, 0), width=1),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.tag = ggtext::element_textbox(size = 10, hjust = 1, color = "#555555", maxheight = 0, halign = 1),
    plot.caption = element_text(size = 10, color = "#555555"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.tag.position = c(1, 0.03),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(2.2, "cm"),
    legend.direction = "horizontal",
    legend.position = 'none'
  )

fn <- str_c('clean-car-states')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))

