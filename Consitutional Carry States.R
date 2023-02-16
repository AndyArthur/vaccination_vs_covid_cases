library(rvest)
library(tidyverse)
library(RSelenium)
library(tigris)

rm(list = ls())

system("killall java")

rs <- rsDriver(
  remoteServerAddr = "localhost",
  port = netstat::free_port(random = T),
  browser = "firefox",
 extraCapabilities = list("moz:firefoxOptions" = list(args = list("--headless"))),
  verbose = F
)

rsc <- rs$client
rsc$navigate('https://worldpopulationreview.com/state-rankings/constitutional-carry-states')

rsc$getPageSource() %>%
  unlist() %>% 
  read_html() %>%
  html_table() %>%
  .[[1]] -> cc

rsc$quit()

usst <-states(cb=T, resolution = '20m') %>% shift_geometry() %>% rmapshaper::ms_simplify()

usst %>%
  inner_join(cc, by=c('NAME'='State')) %>%
  ggplot() + geom_sf(aes(fill=`Constitutional Carry Legal Status`), linewidth=1, color='cornsilk') +
  scale_fill_viridis_d(name='')  +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt">States with Consitutional Carry</span>',
                     '<br /><em style="font-size: 18pt">States that allow people to carry a concealed handgun without a license or permit.</em>'),
       y = "",
       x = "",
       tag=paste('Source: worldpopulationreview.com/state-rankings/constitutional-carry-states<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', width=0.7),
    plot.background = element_rect(fill = "cornsilk", color="cornsilk"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.03),
    legend.position = c(1,1),
    legend.justification = 'right',
    legend.direction = 'horizontal',
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(2.5,'cm'),
    legend.text = element_text(),
  ) 

fn <- str_c('cons-carry-states')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1800, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1800, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))



pop2020 <- tidycensus::get_decennial(geography = "state", variables = "P1_001N",  year = 2020, geometry = F) 

pop2020 %>% inner_join(cc, by=c('NAME'='State')) %>% filter(`Constitutional Carry Legal Status` == 'Legal') %>%
  .$value %>% sum()
