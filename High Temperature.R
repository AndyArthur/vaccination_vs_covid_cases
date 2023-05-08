library(rvest)
library(lemon)
library(tidyverse)
library(lubridate)

rm(list=ls())

for (i in seq(1,6, 1)) {
  url <- paste('https://forecast.weather.gov/product.php?site=ALY&issuedby=ALB&product=CF6&format=TXT&version=',i,'&glossary=0',sep='')
  cf6 <- read_html(url) %>% html_nodes('.glossaryProduct') %>% html_text()
  
  # read the month
  month <- cf6 %>% str_match('MONTH: *(.*)\n')
  month <- month[,2] %>% str_to_title()
  
  # read the year
  year <- cf6 %>% str_match('YEAR: *(.*)\n')
  year <- year[,2] %>% str_to_title()
  
  #clean up the data, skipping first few rows, setting header, then removing the === row, dropping final rows
  tb <- read_table(cf6,skip=16, col_names = T)
  tb <- tb[-1,]
  tb <- tb %>% mutate(across(c(where(is.character), -MIN_1, -PSBL, -WTR, -SNW, -DPTH,  -WX), as.numeric)) %>% drop_na(c(-DR))
  tb <- tb %>% mutate(DATE = as.Date(paste(month, DY, year), format='%B %d %Y'), MO=month, YR=year) %>% relocate(DATE, MO, DY,YR) 
  
  # append to wx table or create wx table
  if (i == 1) {
    wx <- tb
  }
  else {
    wx <- add_row(wx, tb) 
  }
}


wx %>%
  bind_rows(tibble(DATE = seq(as.Date('2023-05-01'),as.Date('2023-05-31'), 'day'))) %>%
  arrange(DATE) %>%
  dplyr::select(DATE, MAX) %>%
  bind_rows(tibble(DATE = as.Date('2023-05-02'), MAX = 56)) %>%
  group_by(DATE) %>%
  slice_max(MAX, n=1) %>%
  ungroup() %>%
  mutate(wkdy = factor(weekdays(DATE, abbreviate=T), levels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')), 
         wkn = week(DATE), 
         mo = month(DATE, label=T, abbr=F),
         day = day(DATE),
         year = year(DATE),
         mo.yr = factor(paste(mo, year),  unique(paste(mo, year))),
         wkn.mo = (5 + day(DATE) +
                     wday(floor_date(DATE, 'month'))) %/% 7
  ) %>%
  ggplot(aes(x=wkdy, y=wkn.mo)) + 
  geom_tile(alpha=0.8, aes(fill=MAX), color='white', linewidth=2) + 
  geom_text(aes(label=day),size=3.5, color='gray40', family="Roboto",hjust=0.5, vjust=0, nudge_y = 0.1) +
  geom_text(aes(label=MAX),size=5,family="Roboto",hjust=0.5, vjust=0, nudge_y = -0.3, fontface='bold') +
  facet_wrap(~mo.yr, scales = 'free_x') +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks=NULL) +
  scale_fill_gradientn(name = "", colors = c("#DC143C", "pink", "violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab",
                       na.value = "grey90", guide = "colourbar", aesthetics = "fill", limits = c(-35,115), seq(-30,115,10)
  ) +
  theme_void() +
  coord_cartesian(expand=F) +
  labs(title="High Temperatures for Albany", 
       caption = str_c('Chart by Andy Arthur on ',format(Sys.time(), "%-m-%-d-%y"),', using ggplot and Albany LCD.')
  ) +
  theme(
    plot.background = element_rect(fill='white', color='white'),
    panel.spacing = unit(20, 'pt'),
    strip.placement = 'outside',
    text = element_text(family = 'Roboto Condensed', size=18),
    plot.caption = element_text(size=12),
    plot.title = element_text( size=50, hjust=0.5, family = 'Roboto Condensed', face='bold', color='orange'),
    axis.text.x = element_text(size=12),
    strip.text = element_text(size=18, face='bold', margin=margin(10,0,6,0)),
    plot.margin = margin(10,10,10,10),
    legend.position = 'none'
  )

fn <- str_c('recent-high-temp')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1820, height=1080, units='px', dpi=130)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1820, height=1080, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


