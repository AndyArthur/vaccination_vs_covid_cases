library(tidyverse)
library(ggtext)

rm(list=ls())

xlf <- tempfile()
download.file('https://www.osc.state.ny.us/files/local-government/publications/excel/local-sales-tax-collections-monthly-quarterly-collections-region.xlsx', xlf)

st <- readxl::read_xlsx(xlf, sheet=1, skip=2)

# clean up heading names
st[1,] %>% pivot_longer(1:ncol(.)) %>% fill(value) %>% pivot_wider() -> st[1,]
st[3,] <- str_c(st[1,], ' - ', st[2,]) %>% as.list()
st %>% janitor::row_to_names(3) %>% janitor::clean_names() -> st

colnames(st) <- c('name','type', colnames(st[3:ncol(st)]))

# make numeric
st <- st %>% mutate(across(3:ncol(.), ~as.numeric(.)))

cities_tax <- st %>% filter(type == 'City') %>% 
  mutate(name = str_c(name, ' city')) %>%
  inner_join(tigris::county_subdivisions('ny', cb=T), ., by=c('NAMELSAD'='name'))

counties_tax <- st %>% filter(type == 'County') %>% 
  mutate(name = str_c(name, ' County')) %>%
  inner_join(tigris::counties('ny', cb=T), ., by=c('NAMELSAD'='name'))

nyco <- tigris::counties('ny',cb=T)

tigris::counties('ny', cb=T) %>% 
  filter(NAME %in% c('Bronx', 'Kings', 'Queens', 'New York', 'Richmond')) %>% 
  group_by() %>% summarise() %>% 
  bind_cols(NAME = 'New York City') %>%
  bind_cols(st %>% filter(name == 'New York City')) %>%
  bind_rows(counties_tax) -> counties_tax

ggplot() + geom_sf(data=counties_tax, aes(fill=january_december_percentage_change),linewidth=0) +
  geom_sf(data=cities_tax, aes(fill=january_december_percentage_change), linewidth=0.1) +
  geom_sf(data=nyco, fill=NA, linewidth=0.6) +
  scale_fill_gradient2(high='darkgreen', labels=scales::label_percent()) +
  coord_sf(expand=F) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 45pt; color: darkgreen">Sales Tax Revenue Growth</span><br /><br/><em>2021 vs.<br />2022'),
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: NYS Comptrollers Office / Department of Tax and Finance'),
       fill = "") +
  theme(
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(2.3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=35, margin=unit(c(10,0,5,0),'pt'), maxheight=0, width=0.4, color='gray30'),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 

fn <- str_c('sales-tax-growth')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

popco <- tidycensus::get_decennial(geography = "county", state='ny', variables = "P1_001N", year = 2020)
popcos <- tidycensus::get_decennial(geography = "county subdivision", state='ny', variables = "P1_001N", year = 2020)

popco %>% filter(NAME %in% str_c(c('Bronx', 'Kings', 'Queens', 'New York', 'Richmond'), ' County, New York')) %>%
  .['value'] %>% sum() -> nycpop

counties_tax <- counties_tax %>% full_join(popco, by='GEOID')
counties_tax <- counties_tax %>%  mutate(value = ifelse(NAME.x == 'New York City', nycpop, value))
cities_tax <- cities_tax %>% inner_join(popcos, by='GEOID')

ggplot() + geom_sf(data=counties_tax, aes(fill=january_november_2022_millions/value), linewidth=0) +
  geom_sf(data=cities_tax, aes(fill=january_november_2022_millions/value), linewidth=0.1) +
  geom_sf(data=nyco, fill=NA, linewidth=0.6) +
  rcartocolor::scale_fill_carto_c(palette = 'BluYl', label=scales::label_dollar(), direction = -1) +
  coord_sf(expand=F) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 45pt; color: darkgreen">Sales Tax Revenue Per Capita</span><br /><br/><em>January - November 2022'),
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: NYS Comptrollers Office / Department of Tax and Finance / US Census 2020 Population'),
       fill = "") +
  theme(
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(2.3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=35, margin=unit(c(10,0,5,0),'pt'), maxheight=0, width=0.4, color='gray30'),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 

fn <- str_c('sales-tax-per-capita')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

