library(lehdr)
library(tidycensus)
library(sf)
library(tidyverse)
library(tigris)
library(ggtext)

ny_lodes_od <- grab_lodes(
  state = "ny",
  year = 2019,
  lodes_type = "od",
  agg_geo = "tract",
  state_part = "main",
  use_cache = TRUE
)

mapview(tracts('ny','albany'))
'36001001100'

ny_lodes_od %>% filter(h_tract == '36001014203') -> dt

cap_adults <- get_acs(
  geography = "tract",
  variables = "S0101_C01_026",
  state = "NY",
  county = c("Albany","Schenectady","Rensselaer","Saratoga"),
  year = 2019,
  geometry = TRUE
)

dt_commuters <- cap_adults %>%
  left_join(dt, by = c("GEOID" = "w_tract")) %>%
  mutate(dt_per_1000 = (S000 / estimate)) 

nyco <- counties('ny') %>% filter(NAME %in% c("Albany","Schenectady","Rensselaer","Saratoga"))
nycos <- county_subdivisions('ny', county = c("Albany","Schenectady","Rensselaer","Saratoga"))

ggplot(dt_commuters) +
  geom_sf(aes(fill=dt_per_1000), linewidth=0) +
  geom_sf(data=nyco, linewidth=0.7, fill=NA, color='white') +
  geom_sf(data=nycos, linewidth=0.1, fill=NA, color='white') +
  scale_fill_viridis_b(labels=scales::label_percent(),  na.value = "black", breaks=c(0,0.5,1,2,4,8,16,32)/100, option = 'G') +
  theme_void() +
  coord_sf(expand = F) +
  labs(
    title = str_c('<span style="color: navy; font-size: 35pt;">Where Do Delmar Residents Work?</span>'),
    y = "",
    x = "",
    caption = "",
    tag = paste("Andy Arthur,", format(Sys.Date(), format = "%-m/%-d/%y"), 
                "<br/>Source: US Census Bureau, LEHD Origin-Destination Employment Statistics (LODES)"),
    fill = ""
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto Condensed", size = 14),
    plot.title = element_textbox(hjust = 0.5, halign=0.5, face = "bold", size = 20, margin = margin(0, 0, 10, 0)),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.tag = ggtext::element_textbox(size = 10, hjust = 1, color = "#555555", maxheight = 0, halign = 1),
    plot.caption = element_text(size = 10, color = "#555555"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.tag.position = c(1, 0.03),
    legend.key.height = unit(2.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.direction = "vertical",
    legend.position = 'left'
  )

fn <- str_c('delmar-work')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))

