library(tidyverse)
library(tigris)
library(sf)
library(ggtext)
rm(list=ls())

library(tidycensus)

hs <- read_csv('https://theunitedstates.io/congress-legislators/legislators-current.csv')

unzip('/home/andy/Downloads/2022 National Congressional Districts for the 118th Congress.zip', exdir='/tmp/')
cd <- read_sf('/tmp/2022 U.S. House of Representatives Districts with Water Clipped to Shoreline.shp') %>% 
  rmapshaper::ms_simplify() %>% 
  st_transform(5070)

cd <- st_make_valid(cd) %>% rmapshaper::ms_simplify()


stfip <- fips_codes %>% select(state, stfips = state_code) %>% unique()

st <- states(cb=T, resolution = '20m') %>% filter(STUSPS != 'PR') %>% shift_geometry()

hs %>% filter(is.na(senate_class)) %>% inner_join(stfip, by=c('state')) %>% 
  mutate(Code = str_c(state, '-', str_pad(replace_na(district,0),2, pad = '0')),
         Code = str_replace(Code,'00','AL')) %>% 
  right_join(cd, by=c('Code')) %>%
  mutate(party = ifelse(is.na(party),'Vacant',party)) %>%
  st_set_geometry('geometry') -> hsm

dp02 <- read_csv('~/Downloads/DP02_1yr_500.csv')
dp02 <- dp02 %>% mutate(GEOID = substr(GEOID, 10, 14)) %>% filter(PROFLN == 18)


hsm %>% mutate(GEOID = str_c(stfips, str_pad(district, 2, 'left', '0'))) %>%
  left_join(dp02, join_by(GEOID)) %>%
  filter(party != 'Vacant') %>%
mutate(PRF_ESTIMATE = parse_number(PRF_ESTIMATE), persqmi = PRF_ESTIMATE/st_area(.) %>% units::set_units('mi^2') %>% units::drop_units() ) %>%
  ggplot() + geom_jitter(aes(y=persqmi, x=party, color=party), size=3) +
  geom_hline(aes(yintercept=1574), color='blue') +
  geom_hline(aes(yintercept=130), color='red') +
    scale_y_continuous(trans = 'log10', labels=scales::label_comma(), name='Residents per square mile') +
  scale_color_manual(values=c('blue','red')) +
  theme_bw() +
  labs(
    title = '<b>Population Density and Party</b> <span style="font-size: 14pt">2023 - 118th Congress</span>',
    subtitle = 'The median Democratic district has 1,574 people per sq mile, while median Republican district has 130 people per sq mile.',
    x='House of Representatives - Party',
    caption='>Source: theunitedstates.io Current Congress List for Party Labels, US Census Bureau for 118th CD TIGER/Line',
    tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme(
    text=element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=32, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0),
    plot.caption=element_text(size=10),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'none',
  ) 

fn <- str_c('party-density')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


hsm %>% mutate(GEOID = str_c(stfips, str_pad(district, 2, 'left', '0'))) %>%
  left_join(dp02, join_by(GEOID)) %>%
  filter(party != 'Vacant') %>%
  mutate(PRF_ESTIMATE = parse_number(PRF_ESTIMATE), persqmi = PRF_ESTIMATE/st_area(.) %>% units::set_units('mi^2') %>% units::drop_units() ) %>%
  st_drop_geometry() %>%
  select(party, persqmi) %>%
  group_by(party) %>%
  summarise(persqmi = median(persqmi))
  


hsm %>% mutate(GEOID = str_c(stfips, str_pad(district, 2, 'left', '0'))) %>%
  left_join(dp02, join_by(GEOID)) %>%
  filter(party != 'Vacant') %>%
  mutate(PRF_ESTIMATE = parse_number(PRF_ESTIMATE), persqmi = PRF_ESTIMATE/st_area(.) %>% units::set_units('mi^2') %>% units::drop_units() ) %>%
  st_drop_geometry() %>%
  select(party, persqmi) %>%
  filter(persqmi > 1574) %>%
  group_by(party) %>%
  summarise(count = n())


library(gt)
hsm %>% mutate(GEOID = str_c(stfips, str_pad(district, 2, 'left', '0'))) %>%
  left_join(dp02, join_by(GEOID)) %>%
  filter(party != 'Vacant') %>%
  mutate(PRF_ESTIMATE = parse_number(PRF_ESTIMATE), persqmi = PRF_ESTIMATE/st_area(.) %>% units::set_units('mi^2') %>% units::drop_units(),
         sqmi = st_area(.) %>% units::set_units('mi^2') %>% units::drop_units() ) %>%
  st_drop_geometry() %>%
  arrange(-persqmi) %>%
  transmute(`First` = first_name, `Last` = last_name, `District` =  Code,  `Party` = party, `Persons Per Sq Mi` = persqmi, `Square Miles` = sqmi) %>%
  head(20) %>%
  gt() %>%
  opt_stylize(color = 'gray') %>%
  fmt_number(5, decimals = 1) %>%
  fmt_integer(6) %>%
  tab_header('Most Densely Populated Congressional Districts, 118th Congress (2023)') %>%
  tab_footnote(html('Andy Arthur, 1/31/23.<br /><em>Data Sources:</em>  theunitedstates.io Current Congress List for Party Labels, US Census Bureau for 118th CD TIGER/Line')) %>%
  gtsave('/tmp/output.png')
