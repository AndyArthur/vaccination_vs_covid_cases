# This code creates a faceted graph and map of life expectancy in America

library(rvest)
library(tidyverse)
library(janitor)

rm(list=ls())
url <- 'https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_life_expectancy'

life <- read_html(url)
table <- life  %>% html_table()

# this is table 2
dlife <- table[[2]]

# remove footnotes
colnames(dlife) <- colnames(dlife) %>% str_replace_all('\\[\\d+\\]','')
dlife <- dlife %>% mutate(across(everything(), str_replace_all, '\\[\\d+\\]','')) 

# convert to long format except the last change column, fix types
dlife <- dlife[-ncol(dlife)] %>% pivot_longer(cols=3:ncol(dlife)-1) %>%
  type_convert()

# clean up column names for easy reference
colnames(dlife) <- colnames(dlife) %>% make_clean_names()

# facet graph of life span by year
dlife %>% ggplot(aes(name, value, fill=value)) + geom_col() +
  scale_x_continuous(expand=c(0,0), breaks=seq(1960,2020,10)) +
  scale_y_continuous(expand=c(0,0)) + 
  facet_wrap(~state_territory) +
  scale_fill_viridis_c(option='B') +
  coord_cartesian(ylim = c(60,85)) +
  theme_minimal() +
  labs(title = 'Life Expectancy, 1960-2019',
       y = "",
       x = "",
       caption=url,
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(text=element_text(family='Overpass',size=14),
      plot.title=element_text(hjust=0.5, face='bold',size=20),
      plot.tag=element_text(size=10,hjust=0, color='#555555'),
      plot.background = element_rect(fill = "white", color="white"),
      plot.caption=element_text(size=10, color='#555555'),
      axis.text.x = element_text(angle=90),
      plot.tag.position = c(0.0,0.01),
      legend.position = 'None',
  )                                 

# save files
filename <- 'Life Expectancy'
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=1600, units='px', dpi=150,device = grDevices::svg)
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=1600, units='px', dpi=150)


# plot a faceted state map
states <- states(cb=T, resolution = '20m') %>% shift_geometry() %>%
  inner_join(dlife, by=c('NAME'='state_territory'))
             
ggplot(states, aes(fill=value)) + geom_sf(size=0.3) +
 scale_fill_viridis_c(breaks=seq(60,90,1)) +
 facet_wrap(~name) +
 theme_void() +
  labs(title = 'Life Expectancy in America',
       y = "",
       x = "",
       caption=url,
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(text=element_text(family='Overpass',size=14),
        plot.title=element_text(hjust=0.5, face='bold',size=26, margin=margin(10,0,10,0)),
        plot.tag=element_text(size=10,hjust=0, color='#555555'),
        plot.background = element_rect(fill = "white", color="white"),
        plot.caption=element_text(size=10, color='#555555'),
        plot.tag.position = c(0.0,0.01),
        legend.position = 'top',
        legend.key.width = unit(5,'cm'),
        legend.key.size = unit(0.25,'cm'),
  )      

filename <- 'Life Expectancy Map'
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=1400, units='px', dpi=150,device = grDevices::svg)
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=1400, units='px', dpi=150)



