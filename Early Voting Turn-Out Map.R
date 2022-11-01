library(tidyverse)
library(googlesheets4)
library(ggtext)
library(lubridate)

rm(list=ls())
uri <- 'https://docs.google.com/spreadsheets/d/1BtDoA8fwV9fybZ_Svn2W_BTolaX4x-or1dgL3q3I_LQ/edit#gid=0'

to <- read_sheet(uri)

tf <- tempfile()
download.file('https://www.elections.ny.gov/NYSBOE/enrollment/county/county_Nov22.xlsx',
              tf)

ce <- readxl::read_xlsx(tf, skip = 3)
ce <- ce %>% filter(STATUS == 'Active', !is.na(COUNTY)) %>%
  mutate(county = str_replace(COUNTY, 'St.Lawrence', 'St. Lawrence'))

to %>% 
  select(county=COUNTY, ev=`Cumul 9`) %>%
  mutate(county = str_replace(county, 'Staten Island', 'Richmond'),
         county = str_replace(county, 'Manhattan', 'New York'),
         county = str_replace(county, 'Brooklyn', 'Kings'),
         county = str_replace(county, 'St.Lawrence', 'St. Lawrence')
  ) -> tom

nyco <- counties('ny', cb=T)

cls <- wacolors::wa_pal('ferries',n=5)[4]

nyco %>% 
  inner_join(tom, by=c('NAME'='county')) %>%
  inner_join(ce, by=c('NAME'='county')) %>%
  mutate(pv = `ev`/TOTAL, pv = cut(pv, breaks=seq(0,1,0.01), labels=scales::percent(seq(0,0.99,0.01), accuracy = 1))) %>%
  ggplot() +
  geom_sf(aes(fill=pv), color='white') +
  wacolors::scale_fill_wa_d(palette = "ferries", guide = "legend") +
  coord_sf(expand=F, crs=3857) +
  theme_void() +
  labs(title = str_c('<span style=\"font-size: 80px; color: ',cls,
                     '\">Early Voters</span><br /> Percentage of Active Enrolled Voters in County that Early Voted through ', format(today()%m-%days(1), format='%A %b %-d.')),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: NYS Active Enrollments and this spreadsheet by @cinyc9\n',uri),
       fill = "")  +
  theme(
    legend.key.height = unit(2,'cm'),
    legend.key.width = unit(1,'cm'),
    legend.position = c(0.1,0.17),
    legend.spacing.y = unit(2, 'cm'),
    legend.direction = 'horizontal',
    legend.justification = 0,
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(25,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) 

fn <- str_c('early-vote')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))


cls <- 'blue'
nyco %>% 
  inner_join(tom, by=c('NAME'='county')) %>%
  inner_join(ce, by=c('NAME'='county')) %>%
  st_transform(3857) %>%
  cartogram::cartogram_ncont('ev', k=2) %>%
  ggplot() + 
  geom_sf(data=nyco, fill=NA, linetype='dashed', size=0.1) +
  geom_sf(aes(fill=DEM/(DEM+REP)), size=0.1) +
  scale_fill_gradient2(name='', low='red', high='blue', midpoint = 0.5, labels=scales::label_percent(), limit=c(0,1)) +
  coord_sf(expand=F, crs=3857) +
  theme_void() +
  labs(title = str_c('<span style=\"font-size: 80px; color: ',cls,
                     '\">Early Voters</span><br /> Color based on active party enrollment, scale based on total early votes through ', format(today()%m-%days(1), format='%A %b %-d.')),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: NYS Active Enrollments and this spreadsheet by @cinyc9\n',uri),
       fill = "")  +
  theme(
    legend.key.height = unit(0.35,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.05,0.17),
    legend.direction = 'horizontal',
    legend.justification = 0,
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(25,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) 

fn <- str_c('early-vote-scale')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))


cls <- 'red'
nyco %>% 
  inner_join(tom, by=c('NAME'='county')) %>%
  inner_join(ce, by=c('NAME'='county')) %>%
  st_transform(3857) %>%
  ggplot() + 
  geom_sf(aes(fill=DEM/(DEM+REP), alpha=ev)) +
  scale_fill_gradient(name='', low='red', high='blue', labels=scales::label_percent(), limit=c(0,1)) +
  coord_sf(expand=F, crs=3857) +
  theme_void() +
  labs(title = str_c('<span style=\"font-size: 80px; color: ',cls,
                     '\">Early Voters</span><br /> Color by active enrollment, saturation based on early votes through ', format(today()%m-%days(1), format='%A %b %-d.')),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: NYS Active Enrollments and this spreadsheet by @cinyc9\n',uri),
       fill = "")  +
  theme(
    legend.key.height = unit(0.35,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.05,0.17),
    legend.direction = 'horizontal',
    legend.justification = 0,
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(25,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) +
  guides(alpha=guide_none())

fn <- str_c('early-vote-alpha')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))

