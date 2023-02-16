library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

cds <- read_sf('~/Downloads/tlgdb_rd22_a_us_legislative.gdb.zip')

cds <- cds %>% rmapshaper::ms_simplify(0.001) %>% shift_geometry() 

usst <- states(resolution = '20m') %>% rmapshaper::ms_simplify() %>% shift_geometry() %>%  filter(GEOID < 58)

dp02 <- read_csv('~/Downloads/DP02_1yr_500.csv')

dp02 <- dp02 %>% mutate(GEOID = substr(GEOID, 10, 14)) %>% filter(PROFLN == 18)

cds %>% filter(GEOID < 5800) %>%
  left_join(dp02, join_by(GEOID)) %>%
  mutate(PRF_ESTIMATE = parse_number(PRF_ESTIMATE),depart = PRF_ESTIMATE-(sum(PRF_ESTIMATE, na.rm=T)/435)) %>%
  drop_na(PRF_ESTIMATE) %>%
  ggplot() + geom_sf(aes(fill=depart), color='gray80') + scale_fill_steps2(labels=scales::label_comma(), n.breaks=8) +
  geom_sf(data=usst, fill=NA, linewidth=0.4, color='gray45') + 
  theme_void() +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt; color: darkblue">Over</span> and ',
                     '<span style="font-size: 45pt; color: darkred">Under</span> ',
                     '<span style="font-size: 45pt;">Populated</span><br />',
                     '<span style="font-size: 26pt">2023 Congressional Districts</span> <br />',
                     'Redistricting is supposed to make congressional districts equal in population across each state, however the formula that allocates districts to states makes some states malapportioned, along with population changes since 2020.'),
       y = "",
       x = "",
       tag=paste('2021 American Community Survey (ACS) 1-year estimates for 118th Congress.<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=12, width=0.5, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', maxheight=0, halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.03),
    legend.position = c(0.8,1.04),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
  ) 

fn <- str_c('congress-pop')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1400, units='px', dpi=130)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

