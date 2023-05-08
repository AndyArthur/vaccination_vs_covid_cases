library(tidyverse)
library(lubridate)

rm(list=ls())
tigris::county_subdivisions('ny') %>% filter(NAME == 'Albany') %>% 
  st_transform(4326) %>%
  st_centroid() %>%
  mutate(lat = st_coordinates(.)[,2], lng = st_coordinates(.)[,1]) %>%
  st_drop_geometry() %>% select(lat, lng) -> albcenter

suncalc::getSunlightTimes(date = seq(as.Date('2023-05-01'), as.Date('2023-05-31'), 'days'), lat = albcenter$lat, lon  = albcenter$lng) %>%
  mutate(wkdy = factor(weekdays(date, abbreviate=F), levels=c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')), 
         wkn = week(date), 
         mo = month(date, label=T, abbr=F),
         mon = month(date),
         day = day(date),
         sunsetc = hm(format(sunset,  format = '%H:%M', tz='America/New_York')) %>% as.numeric - 60*60*19
         ) %>% 
  group_by(mo) %>% 
  mutate(wkn.mo = dense_rank(wkn)) %>%
  ungroup() %>% 
  ggplot(aes(x=wkdy, y=wkn.mo)) +
  geom_tile(alpha=0.8, aes(fill=sunsetc), width=0.9, height=0.9) + 
  geom_text(aes(label=day),size=20,family="American Typewriter",hjust=0.5, vjust=0, nudge_y = 0) +
  geom_text(aes(label=format(sunset, '%-I:%M %p', tz='America/New_York')),size=8,family="American Typewriter",
            hjust=0.5, vjust=0, nudge_y = -0.3) +
  scale_fill_viridis_c(option = 'C', begin = 0.6) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks=NULL) +
  theme_void() +
  coord_cartesian(expand=F, ylim=c(5.6, 0.4)) +
  labs(title="May 2023 Sunsets in Albany", 
       x="", y="",
       caption = 'Andy Arthur, 3/13/2023.'
  ) +
  theme(
    plot.background = element_rect(fill='white', color='white'),
    strip.placement = 'outside',
    text = element_text(family = 'American Typewriter', size=40),
    plot.caption = element_text(size=12),
    plot.title = element_text(face='bold', size=60, hjust=0.5, margin=margin(0,0,20,0)),
    axis.text.x = element_text(size=20),
    plot.margin = margin(10,10,10,10),
    legend.position = 'none'
  )


fn <- str_c('march-sunset')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1080, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

