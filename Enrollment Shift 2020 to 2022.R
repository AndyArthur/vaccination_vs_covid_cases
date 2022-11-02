library(tidyverse)
library(googlesheets4)
library(ggtext)
library(lubridate)

rm(list=ls())

nyco <- counties('ny', cb=T, resolution = '20m')

to <- read_sheet(uri)

tf <- tempfile()
download.file('https://www.elections.ny.gov/NYSBOE/enrollment/county/county_Nov22.xlsx',
              tf)

ce22 <- readxl::read_xlsx(tf, skip = 3)
ce22 <- ce22 %>% filter(STATUS == 'Active', !is.na(COUNTY)) %>%
  mutate(county = str_replace(COUNTY, 'St.Lawrence', 'St. Lawrence'))

tf <- tempfile()
download.file('https://www.elections.ny.gov/NYSBOE/enrollment/county/county_Nov20.xlsx',
              tf)

ce20 <- readxl::read_xlsx(tf, skip = 3)
ce20 <- ce20 %>% filter(STATUS == 'Active', !is.na(COUNTY)) %>%
  mutate(county = str_replace(COUNTY, 'St.Lawrence', 'St. Lawrence'))


nyco %>% inner_join(ce20, by=c('NAME'='county')) %>%
  inner_join(ce22,by=c('NAME'='county'), suffix=c('.20','.22')) %>%
  mutate(DEM.chg = DEM.22-DEM.20,
         REP.chg = REP.22-REP.20,
         per.chg =  (DEM.22-DEM.20)/(DEM.20+REP.20)
  ) %>%
  ggplot() +
  geom_sf(aes(fill=per.chg)) +
  scale_fill_steps2(low='red',high='blue', labels=scales::label_percent(), breaks=seq(-1,1,0.01), guide = 'colorbar') +
  coord_sf(expand=F, crs=3857) +
  theme_void() +
  labs(title = str_c('<span style=\"font-size: 80px; color: red',
                     '\">Enrollment Shifts</span><br /> November 2020 versus 2022, ratio of enrolled Democrats to Republicans'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: NYS Active Enrollments Nov 2020 vs Nov 2022'),
       fill = "")  +
  theme(
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.position = c(0.1,0.17),
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

fn <- str_c('enroll-shift-22')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))

