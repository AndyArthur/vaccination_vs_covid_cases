library(rvest)
library(tidyverse)

# clear the environment
rm(list=ls())

# get a years worth of CF-6 reports, append together
for (i in seq(1,13, 1)) {
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
  tb <- tb %>% mutate(across(c(where(is.character), -MIN_1, -PSBL, -WTR, -SNW, -DPTH), as.numeric)) %>% drop_na(c(-DR))
  tb <- tb %>% mutate(DATE = as.Date(paste(month, DY, year), format='%B %d %Y'), MO=month, YR=year) %>% relocate(DATE, MO, DY,YR) 

  # append to wx table or create wx table
  if (i == 1) {
    wx <- tb
  }
  else {
    wx <- add_row(wx, tb) 
  }
}

# finally, sort by date
wx <- wx %>% arrange(DATE)

# find last 90 days
library(lubridate)
days90 <- wx %>% filter(DATE > today() %m-% days(91))

# graph departure from normal
ggplot(days90, aes(DATE, DEP, fill=DEP)) + geom_hline(yintercept = 0, size=2, color="#413e4f") + geom_col() + 
  scale_fill_gradient2(low='blue',high='red',mid = 'white', breaks=seq(-30,30,5)) +
  scale_x_date(date_breaks = 'week', date_minor_breaks = 'day', date_labels = '%b %-d', expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-30,30,2)) +
  theme_minimal() +
  labs(title = 'Departure from Normal, Past 90 Days',
       subtitle='Despite recent cold weather, it\'s been a pretty mild autumn and winter so far...',
       y = "",
       x = "",
       caption='National Weather Service, CF-6 Reports',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(
    text= element_text(family='Overpass',size=14, color="#eeeedd"),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    axis.title = element_text(color='#413e4f'),
    plot.background = element_rect(fill = "#413e4f", size=0),
    panel.background = element_rect(fill = "#413e4f", size=0),
    axis.text = element_text(color='white'),
    panel.grid.minor = element_line(size=0.1),
    #axis.line = element_line(size=0.5, color='#EEEEEE'),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='#EEEEEE'),
    plot.caption=element_text(size=10, color='#EEEEEE'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'top',
  ) +
  guides(fill = guide_colourbar(barwidth = 50, barheight = 0.7,ticks = FALSE, size=0.3,
                                frame.colour = "#333333", label.position = "bottom"))


ggsave('Desktop/Depature.jpg', width=1920, height=1600, units='px', dpi=150)
ggsave('Desktop/Departure.svg', width=1920, height=1600, units='px', dpi=150, device = grDevices::svg)

# graph the high temperature, colored by departure from normal
ggplot(days90, aes(DATE, MAX, fill=DEP)) + geom_hline(yintercept = 0, size=2, color="#413e4f") + geom_col() + 
  scale_fill_gradient2(low='blue',high='red',mid = 'white', breaks=seq(-30,30,5),name='Departure\nfrom Normal') +
  scale_x_date(date_breaks = 'week', date_minor_breaks = 'day', date_labels = '%b %-d', expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-300,300,5),  expand=c(0,0)) +
  coord_cartesian(ylim = c(20,80)) +
  theme_minimal() +
  labs(title = 'High Temperature',
       y = "",
       x = "",
       caption='National Weather Service, CF-6 Reports',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(
    text= element_text(family='Overpass',size=14, color="#eeeedd"),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    axis.title = element_text(color='#413e4f'),
    plot.background = element_rect(fill = "#413e4f", size=0),
    panel.background = element_rect(fill = "#413e4f", size=0),
    axis.text = element_text(color='white'),
    panel.grid.minor = element_line(size=0.1),
    #axis.line = element_line(size=0.5, color='#EEEEEE'),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='#EEEEEE'),
    plot.caption=element_text(size=10, color='#EEEEEE'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'top',
  ) +
  guides(fill = guide_colourbar(barwidth = 50, barheight = 0.7,ticks = FALSE, size=0.3,
                                frame.colour = "#333333", label.position = "bottom")) -> fx

ggsave('Desktop/High Temp.jpg', width=1920, height=1600, units='px', dpi=150)
ggsave('Desktop/High Temp.svg', width=1920, height=1600, units='px', dpi=150, device = grDevices::svg)

# export to plotly to make interactive for the blog
library(plotly)
ggplotly(fx)

fx %>% plotly_json(FALSE) %>% str_replace_all("\n",'') %>% write('/tmp/High Temperature, Last 90 Days Before January 10, 2022.jsx')
