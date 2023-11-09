library(tidyverse)
library(sf)
library(ggtext)
library(data.table)

tmpD <- tempdir()
archive::archive_extract('~/Downloads/eia8602022.zip', dir = tmpD)

pp <- readxl::read_xlsx(str_c(tmpD,'/2___Plant_Y2022.xlsx'), skip=1)
gen <- readxl::read_xlsx(str_c(tmpD, '/3_1_Generator_Y2022.xlsx'), skip=1)

fuel  <- readxl::read_xlsx(str_c(tmpD, '/LayoutY2022.xlsx'), skip=2, sheet=2)[,4:5]

pp <- pp %>% inner_join(gen) %>%
  left_join(fuel, by=c('Energy Source 1'='Prime Mover Code'))

nyco <- tigris::counties('ny', cb=T) %>% st_transform(4326)
nycos <- tigris::county_subdivisions('ny',cb=T)

pp %>% filter(State=='NY', Latitude>40, Technology == 'Solar Photovoltaic') %>%
  group_by(`Plant Code`) %>%
  summarise(TotalMW = sum(`Winter Capacity (MW)`), Technology = first(Technology), Longitude=first(Longitude), Latitude = first(Latitude)) %>%
  ungroup() %>%
  as.data.table() %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs=4326) %>%
  st_transform(3857) %>%
  st_jitter(1000) %>%
  arrange(`TotalMW`) %>%
  ggplot() + 
  geom_sf(data=nyco, fill=NA, linewidth=0.3, color='gray80') +
  geom_sf(data=nycos, fill=NA, linewidth=0.05, color='gray80') +
  geom_sf(aes(size=`TotalMW`), shape=21, stroke=0.3, alpha=0.7, fill='yellow', color='white') +
  scale_size(name='Total MW', breaks = seq(0,30,5), range = c(0.5,12))+
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 58pt; color: lightyellow">Utility-Scale Industrial Solar</span><br /><br />in New York State (2022)</em>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: EIA 860 Data (2022)'),
       fill = "")  +
  theme(
    legend.position = c(0.95,0.5),
    legend.justification = c(0.5,0.5),
    text= element_text(family='Roboto Condensed',size=14, color='gray90'),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "gray10", color="gray10"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) +
  guides( fill = guide_legend(override.aes = list(size=6)),
          size =  guide_legend(override.aes = list()))


fn <- 'Utility-Scale_Industrial_Solar'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=140)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=140, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
source('upload-svg.R', local=T)

pp %>% filter(State=='NY', Latitude>40, Technology == 'Solar Photovoltaic') %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs=4326) %>%
  write_sf('/tmp/usolar.gpkg')
