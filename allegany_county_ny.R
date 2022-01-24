library(rvest)
library(tidyverse)
library(tigris)
library(zoo)
library(classInt)
library(janitor)

rm(list=ls())
url <- 'https://en.wikipedia.org/wiki/Allegany_County,_New_York'

countyinfo <- read_html(url)

table <- countyinfo %>% html_table()

pop <- table[[2]] %>% row_to_names(1) %>% clean_names() %>% 
  mutate(across(everything(), str_replace_all, '−','-')) %>%
           mutate(across(everything(), parse_number ))

pop$scale <- na.fill(ifelse(as.numeric(gsub("[^0-9.-]","", gsub("−",'-',pop$percent)))>0, T, F),F)

ggplot(pop, aes(census,pop)) + geom_col(aes(fill=scale)) +  geom_line() + 
  geom_label(aes(label=paste(scales::comma(pop,accuracy=1),str_c(percent,'%'),sep='\n')),
             label.size=0.2, label.padding =unit(0.1,'lines'), size=3, vjust=0, nudge_y=(max(pop$pop,na.rm = T)*0.02)) + 
  scale_fill_manual(values=c('red','green2')) +
  scale_y_continuous(name='Population', expand = expansion(mult = clibrary(rvest)
library(tidyverse)
library(tigris)
library(zoo)
library(classInt)
library(janitor)

rm(list=ls())
url <- 'https://en.wikipedia.org/wiki/Allegany_County,_New_York'

countyinfo <- read_html(url)

table <- countyinfo %>% html_table()

pop <- table[[2]] %>% row_to_names(1) %>% clean_names() %>% 
  mutate(across(everything(), str_replace_all, '−','-')) %>%
           mutate(across(everything(), parse_number ))

pop$scale <- na.fill(ifelse(as.numeric(gsub("[^0-9.-]","", gsub("−",'-',pop$percent)))>0, T, F),F)

ggplot(pop, aes(census,pop)) + geom_col(aes(fill=scale)) +  geom_line() + 
  geom_label(aes(label=paste(scales::comma(pop,accuracy=1),str_c(percent,'%'),sep='\n')),
             label.size=0.2, label.padding =unit(0.1,'lines'), size=3, vjust=0, nudge_y=(max(pop$pop,na.rm = T)*0.02)) + 
  scale_fill_manual(values=c('red','green2')) +
  scale_y_continuous(name='Population', expand = expansion(mult = c(0, .1)), labels=scales::comma_format(accuracy=1)) +
  scale_x_continuous(expand = c(0, 0),name='Year', breaks=seq(1700,2050,10))+
  labs(title=paste('Allegany County, NY Population',sep=''),
       caption=url,
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%-d/%y")),
  ) +
  theme_minimal() + theme(text=element_text(family='Overpass',size=14),
                          plot.title=element_text(hjust=0.5, face='bold',size=20),
                          plot.subtitle=element_text(hjust=0.5),
                          plot.tag=element_text(size=10,hjust=0, color='#555555'),
                          plot.caption=element_text(size=10, color='#555555'),
                          plot.tag.position = c(0.0,0.01),
                          panel.border = element_blank(),
                          legend.position = 'none',
                          panel.spacing.y = unit(c(0,0,0,0), 'lines'),
                          plot.background = element_rect(fill = "white", color="white"),
                          plot.margin = unit(c(1,1,1,1), 'lines'),
                          axis.text  = element_text(family='Overpass Mono', color='#333333', size=8),
                          axis.title.y = element_blank(),
                          axis.ticks.x = element_blank(),
                          #axis.title  = element_text(family='Overpass Mono', color='#333333'),
  )

city <- 'Allegany County Population'
ggsave(paste('/tmp/',city,'.svg',sep=''), width=1920, height=1600, units='px', dpi=150,device = grDevices::svg)

(0, .1)), labels=scales::comma_format(accuracy=1)) +
  scale_x_continuous(expand = c(0, 0),name='Year', breaks=seq(1700,2050,10))+
  labs(title=paste('Allegany County, NY Population',sep=''),
       caption=url,
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%-d/%y")),
  ) +
  theme_minimal() + theme(text=element_text(family='Overpass',size=14),
                          plot.title=element_text(hjust=0.5, face='bold',size=20),
                          plot.subtitle=element_text(hjust=0.5),
                          plot.tag=element_text(size=10,hjust=0, color='#555555'),
                          plot.caption=element_text(size=10, color='#555555'),
                          plot.tag.position = c(0.0,0.01),
                          panel.border = element_blank(),
                          legend.position = 'none',
                          panel.spacing.y = unit(c(0,0,0,0), 'lines'),
                          plot.background = element_rect(fill = "white", color="white"),
                          plot.margin = unit(c(1,1,1,1), 'lines'),
                          axis.text  = element_text(family='Overpass Mono', color='#333333', size=8),
                          axis.title.y = element_blank(),
                          axis.ticks.x = element_blank(),
                          #axis.title  = element_text(family='Overpass Mono', color='#333333'),
  )

city <- 'Allegany County Population'
ggsave(paste('/tmp/',city,'.svg',sep=''), width=1920, height=1600, units='px', dpi=150,device = grDevices::svg)

