library(tidyverse)
library(suncalc)
library(lubridate)

rm(list=ls())

getMoonIllumination(date = seq(ISOdate(2023,1,1), ISOdate(2023,12,31), by = 'hour'), 
                                keep = c("fraction")) %>%
  mutate(
        full = (lag(fraction) < fraction & lead(fraction) < fraction)
        ) %>%
  filter(full) %>% 
  transmute(
    date =  as.Date(date - hours(5)),
    full = TRUE
  ) -> fullmoon


tibble(
  date = seq(as.Date('2023-01-01'), as.Date('2023-12-31'), 'day')
) %>%
  left_join(fullmoon) %>%
  mutate(wkdy = factor(weekdays(date, abbreviate=T), levels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')), 
         wkn = week(date), 
         mo = month(date, label=T, abbr=F),
         day = day(date)) %>%
  group_by(mo) %>% 
  mutate(wkn.mo = dense_rank(wkn)) %>%
  ungroup()  %>%
  ggplot(aes(x=wkdy, y=wkn.mo)) +
  geom_tile(alpha=0.8, aes(fill=full), color='white', linewidth=2) + 
  geom_text(aes(label=paste0(day), color=full),size=3.4,family="Roboto",hjust=0.5,) +
  facet_wrap(~mo) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks=NULL) +
  scale_fill_manual(values=c('pink'),name ='', na.value='white') +
  scale_color_manual(values=c('black'), na.value = 'gray30') +
  theme_void() +
  coord_equal(expand=F) +
  labs(title = 'Full Moon in 2023',
       caption = paste('Chart by Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%-y"))
  ) +
  theme(
    plot.background = element_rect(fill='white', color='white'),
    panel.spacing = unit(10, 'pt'),
    strip.placement = 'outside',
    text = element_text(family = 'Roboto', size=18),
    plot.caption = element_text(size=12),
    plot.title = element_text(face='bold', size=40, hjust=0),
    axis.text.x = element_text(size=12),
    strip.text = element_text(size=18, face='bold', margin=margin(10,0,6,0)),
    plot.margin = margin(20,20,20,20),
    legend.position = 'None'
  )



