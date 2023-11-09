library(tidyverse)
library(sf)
library(ggtext)
library(data.table)

tmpD <- tempdir()
archive::archive_extract('~/Downloads/eia8602022.zip', dir = tmpD)

pp <- readxl::read_xlsx(str_c(tmpD,'/2___Plant_Y2022.xlsx'), skip=1)
gen <- readxl::read_xlsx(str_c(tmpD, '/3_1_Generator_Y2022.xlsx'), skip=1)

fuel  <- readxl::read_xlsx(str_c(tmpD, '/LayoutY2022.xlsx'), skip=2, sheet=2)[,4:5]

pp <- pp %>% inner_join(gen, by='Plant Code') %>%
  left_join(fuel, by=c('Energy Source 1'='Prime Mover Code'))

usco <- tigris::counties(resolution = '20m', cb=T) %>% tigris::shift_geometry() %>% st_transform(4326) %>% filter(STUSPS!='PR')
uss <-  tigris::states(resolution = '20m', cb=T) %>% tigris::shift_geometry() %>% st_transform(4326) %>% filter(STUSPS!='PR')
#nycos <- tigris::county_subdivisions(cb=T)

pp %>% filter(Technology == 'Solar Photovoltaic') %>%
  group_by(`Plant Code`) %>%
  summarise(TotalMW = sum(`Winter Capacity (MW)`), Technology = first(Technology), Longitude=first(Longitude), Latitude = first(Latitude)) %>%
  ungroup() %>%
  as.data.table() %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs=4326) %>%
  tigris::shift_geometry() %>%
  st_transform(3857) %>%
  st_jitter(1000) %>%
  arrange(`TotalMW`) %>%
  ggplot() + 
  geom_sf(data=usco, fill=NA, linewidth=0.05, color='gray80') +
  geom_sf(data=uss, fill=NA, linewidth=0.5, color='gray80') +
  geom_sf(aes(size=`TotalMW`), shape=21, stroke=0.3, alpha=0.9, fill='yellow', color='white') +
  scale_size(name='Total MW', range = c(0.3,8), breaks = c(50,150,250,500))+
  theme_void() +
  coord_sf(expand=F, crs=5070) +
  labs(title = str_c('<span style="font-size: 38pt; color: yellow">Utility-Scale Industrial Solar (2022)</span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: EIA 860 Data (2022)'),
       fill = "")  +
  theme(
    legend.position = c(0.5,1),
    legend.justification = c(0.5,1),
    legend.direction = 'horizontal',
    text= element_text(family='Roboto Condensed',size=14, color='gray90'),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=25),
    plot.background = element_rect(fill = "gray10", color="gray10"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10, hjust=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1.0,0.01),
  ) +
  guides( fill = guide_legend(override.aes = list(size=6)),
          size =  guide_legend(override.aes = list()))

fn <- 'Utility-Scale_Industrial_Solar_in_the_USA'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=140)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=140, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
source('upload-svg.R', local=T)
