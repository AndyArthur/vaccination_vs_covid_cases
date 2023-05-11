library(tidyverse)
library(sf)

tom.aqi <- arcpullr::get_spatial_layer('https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/AirNowAQIForecast/FeatureServer/1')

# this builds a dataframe of styles/colors from feature server
feature.info <- jsonlite::read_json('https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/AirNowLatestContoursCombined/FeatureServer/0/?f=pjson')
style <- map(feature.info$drawingInfo$renderer$uniqueValueInfos,
    \(x) {
      tibble(value = x$value %>% as.numeric, label = x$label,
             color = rgb(x$symbol$color[[1]], x$symbol$color[[2]], x$symbol$color[[3]], x$symbol$color[[4]], maxColorValue = 255))
    }
  ) %>% list_rbind()

usst <- tigris::states(cb=T, resolution = '20m') %>% filter(GEOID < 57, !STUSPS %in% c('AK','HI')) %>% st_transform(5070)
usco <- tigris::counties(cb=T, resolution = '20m') %>% filter(GEOID < 57, !STUSPS %in% c('AK','HI')) %>% st_transform(5070)

tom.aqi <- tom.aqi %>% st_transform(5070) %>% st_intersection(usst)

tom.aqi <- tom.aqi %>%
  group_by(gridcode) %>%
  summarize(percent = sum(st_area(geoms))/sum(st_area(.)) %>% units::drop_units()) %>%
  inner_join(style, join_by(gridcode == value)) %>%
  mutate(label = str_c(label, ' (', round(percent*100, 1),'%)'))

tom.aqi  %>%
  ggplot() +
  geom_sf(aes(fill=label), color='white', linewidth=0.05) +
  geom_sf(data=usst, fill=NA) +
  geom_sf(data=usco, fill=NA, linewidth=0.05) +
    scale_fill_manual(values = style$color) +
  coord_sf(crs = 5070, expand=F) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 45pt">Air Quality Forecast for May 12, 2023</span><br />',
                     'Tomorrow air quality is expected to decline in much of NY State, though it will clear out in the Mid-West.'
                     ),
         y = "",
         x = "",
         tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%d/%y"),
                   '<br />Source: AirNow.gov'),
         fill = "")  +
    theme(
      text= element_text(family='Roboto Condensed',size=14),
      plot.title=ggtext::element_textbox_simple(hjust=0, halign=0, face='bold', margin=margin(0,0,20,0)),
      plot.background = element_rect(fill = "snow", color="snow"),
      plot.tag=ggtext::element_textbox(size=12,hjust=0, color='#555555', maxheight=0, halign = 0, valign=0),
      plot.margin = unit(c(1,1,1,1), 'lines'),
      plot.tag.position = c(0,0),
      legend.text = element_text(size=16),
      legend.position = c(1,1),
      legend.direction = 'horizontal',
      legend.justification = c(1,1),
      legend.key.height = unit(0.7,'cm'),
      legend.key.width = unit(0.7,'cm')
    )

  fn <- str_c('air-tom')
  ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
  ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
  system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))




