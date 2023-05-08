library(tidyverse)
library(suncalc)
library(lubridate)

rm(list=ls())

Moon_2023 <-getMoonIllumination(date = seq.Date(as.Date("2023-05-01"), as.Date("2023-05-31"), by = 1), 
                                keep = c("fraction", "phase", "angle"))

Moon_2023_df <- Moon_2023 %>% 
  mutate(wkdy = factor(weekdays(date, abbreviate=T), levels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')), 
         wkn = week(date), 
         mo = month(date, label=T, abbr=T),
         day = day(date), 
         moon.phase = cut(
           phase,breaks=seq(0,1,by=1/56),
           ordered_result=T)) %>%
  group_by(mo) %>% 
  mutate(wkn.mo = dense_rank(wkn)) %>%
  ungroup()

moon..tb <- tibble(
  moon. = c(NA, letters[rep(1:13,rep(2,13))], 0,0, letters[rep(14:26,rep(2,13))],NA),
  moon.phase = levels(Moon_2023_df$moon.phase)
)

Moon_2023_df %>% left_join(moon..tb)  %>% ggplot(aes(x=wkdy, y=wkn.mo)) + 
  geom_tile(alpha=0.8, aes(fill=fraction), color='white', linewidth=2) + 
  geom_text(aes(label=paste0(day)),size=20,family="American Typewriter",hjust=0, color="white", nudge_x=0.01) +
  geom_text(aes(label=moon.), family="Moon Phases", hjust=1, nudge_x = -0, color="#ffeeee", size=26) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks=NULL) +
  scale_fill_gradient(name="Moon Illumination", low = 'black', high='#330066', guide="none") +  ## so that sky is darkest when there's new moon
  theme_void() +
  coord_cartesian(expand=F) +
  labs(title="Moon Phase Calendar - May 2023", x="", y="",
       caption = 'Chart by Andy Arthur, 5/4/2023. Created using ggplot and rSuncalc.'
       ) +
  theme(
    plot.background = element_rect(fill='white', color='white'),
    text = element_text(family = 'American Typewriter'),
    plot.caption = element_text(size=12),
    plot.title = element_text(face='bold', size=50, hjust=0.5),
    axis.text.x = element_text(size=30, margin=margin(30,0,10,0)),
    plot.margin = margin(10,10,10,10)
  )


fn <- str_c('may-moon-phase')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1080, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

