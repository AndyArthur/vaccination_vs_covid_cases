library(tidyverse)
library(tigris)
library(sf) 
library(ggtext)
library(archive)
library(lubridate)

rm(list=ls())

dolzip <- 'https://dol.ny.gov/statistics-lauszip'

ueRate <- rbind(
  read_csv(archive_read(dolzip, 'laus_cities.txt')),
  read_csv(archive_read(dolzip, 'laus_counties.txt'))
) 

ueRate %>%
  mutate(
    AREA = str_replace(AREA, 'City','city'),
    AREA = str_replace(AREA, 'Town','town'),
    AREA = str_replace(AREA, 'Village','village'),
    AREA = str_replace(AREA, ' Ny',''),
    ) -> ueRate

ueRate %>% filter(MONTH != '00') %>%
  mutate(DATE=ym(str_c(YEAR, MONTH))) %>%
  filter(DATE==max(DATE)) -> ueRateM

counties <- counties(state='ny', cb=T)
cousub <- county_subdivisions(state='ny', cb=T) %>% rmapshaper::ms_simplify()
countiesmerge <- st_sf(st_union(counties))


ueRateM %>% 
  right_join(county_subdivisions('ny', cb=T), ., by=c('NAMELSAD' = 'AREA')) %>%
  filter(!(NAMELSADCO=='Franklin County' & NAMELSAD=='Brighton town')) -> ucs

ueRateM %>% 
  right_join(counties('ny', cb=T), ., by=c('NAMELSAD' = 'AREA')) -> uc

ueRateM %>% 
  right_join(places('ny', cb=T), ., by=c('NAMELSAD' = 'AREA')) -> uv

nys <- counties('ny',cb=T) %>% st_union()

ggplot() + 
  geom_sf(data = uc, aes(fill = UNEMPRATE), linewidth=0.1) +
  geom_sf(data = ucs, aes(fill = UNEMPRATE), linewidth=0.01) +
  geom_sf(data = uv, aes(fill = UNEMPRATE), linewidth=0.01) +
  geom_sf(data = nys, fill=NA, linewidth=1) +
  geom_sf(data=counties, size=0.3, fill=NA)+
  scale_fill_fermenter(palette = "RdYlBu", 
                       direction = -1, breaks=seq(0,20,0.5),
                       label=scales::label_percent(scale=1)
                       ) + 
  geom_sf(data=cousub, linewidth=0.03, fill=NA, color='black') +
  geom_sf(data=countiesmerge, fill=NA, size=0.6)+
  coord_sf(expand=F) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 55pt">NYS Unemployment Rate</span><br /><br/><em>',format(max(ueRateM$DATE),'%B %Y')),
       y = "",
       x = "",
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: NYS Department of Labor, ', dolzip ),
       fill = "") +
  theme(
    legend.key.height = unit(1.5,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=35, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 

fn <- str_c('unemployment-rate',ueRateM$DATE[1])
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


ueRate %>% 
  filter(MONTH == '00', YEAR>2010) %>%
  inner_join(counties('ny',cb=T, resolution='20m'), ., by=c('NAMELSAD' = 'AREA')) %>%
  ggplot() + geom_sf(aes(fill=UNEMPRATE), linewidth=0.1) + 
  facet_wrap(~YEAR) +
  scale_fill_fermenter(palette = "RdYlGn", 
                       direction = -1, breaks=seq(0,20,2), label=scales::label_percent(scale=1)) + 
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 45pt">Unemployment Rate</span><br />',
                     'NYS Department of Labor, Local Area Unemployment Statistics,  ',
                 'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y.")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, size=12, face='bold', margin=margin(b=10), width=1),
    plot.background = element_rect(fill = "#FFFFFF", color="#FFFFFF"),
    plot.tag=ggtext::element_textbox(size=12, color='#555555', maxheight=0, halign = 1, hjust=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.76,0.03),
    strip.text = element_text(face='bold'),
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(0.9,1.06)
  ) 

fn <- str_c('unemployment-rate-yr')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

library(gt)
library(lubridate)
library(gtExtras)
uc %>%
  st_drop_geometry() %>%
  select(County = NAME, Rate = UNEMPRATE) %>%
  drop_na() %>%
  arrange(County) %>%
  gt()


RColorBrewer::brewer.pal(


ueRate %>% filter(YEAR > 2021, grepl('County',AREA)) %>% .$UNEMPRATE %>% min

ueRate %>% filter(YEAR > 2021, grepl('County',AREA)) %>%
  mutate(DATE=ym(str_c(YEAR, MONTH))) %>%
  #select(across(2:ncol(.), ~str_replace(., '2022-',''))) %>% 
  pivot_wider(id_cols = AREA, names_from = DATE, values_from = UNEMPRATE) %>%
  gt() %>%
  gt_color_rows(2:13, domain = c(1.9, 10.3), palette = rev(RColorBrewer::brewer.pal(10, 'RdYlGn')))%>%
  fmt_percent(2:13, scale_values = F, decimals = 1) %>%
  opt_stylize(color = 'gray') %>%
  opt_css('body { text-align: center} ') %>%
  cols_align('center') %>%
  tab_header('New York State Unemployment Rate, 2022',
             'Data is not seasonally adjusted.') %>%
  tab_footnote(html('Andy Arthur, 1/24/23.<br /><em>Data Source:</em> NYS Department of Labor, Local Area Unemployment Statistics.')) %>%
  gtsave('/tmp/output.html')

