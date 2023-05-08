library(tidyverse)
library(tidycensus)
library(tigris)
library(ggtext)
library(sf)
library(archive)
library(openxlsx)

rm(list=ls())

tmpF <- tempfile()
tmpD <- tempdir()

# get latfor population adjusted data
options(timeout = max(300, getOption("timeout")))
download.file('https://www.latfor.state.ny.us/data/2020files/population_data_files.zip', tmpF)
unzip(tmpF, exdir = tmpD)

latfor.adj.pop <- read.xlsx(str_c(tmpD,'/Population Data Files/PL_ADJUSTED_BLOCK.xlsx'),  startRow=2, cols=1:10)

op <-  get_decennial(geography = "block", variables = "P1_001N", 
                     year = 2020, cache=T, geometry = T, state='ny') %>%
  st_transform(5070) %>% 
  st_centroid() %>%
  left_join(latfor.adj.pop, 
            join_by(GEOID == BLKID)) %>%
  mutate(Prisoners = TOTAL_ADJ - value ) %>%
  filter(Prisoners >= 0) # Negative values exists in blocks with state prisons

act <- tracts('ny', c('albany','rensselaer','schenectady','saratoga'), year=2020) %>% 
  st_transform(5070) 

acs <- county_subdivisions('ny', c('albany','rensselaer','schenectady','saratoga'))

op %>% st_intersection(act) %>%
  st_drop_geometry() %>%
  group_by(GEOID.1) %>%
  summarise(Pop = sum(value), Prisoners = sum(Prisoners), PP = sum(Prisoners)/sum(value)*1000) %>%
  left_join(act, ., join_by(GEOID == GEOID.1 )) %>%
  ggplot() + 
  geom_sf(aes(fill=PP), linewidth=0) +
  geom_sf(data=acs, fill=NA, linewidth=0.3, color='white') +
  scale_fill_viridis_b(n.breaks=10, option='F',name='') +
  coord_sf(crs=3857, expand=F) +
  labs(
    title = 'Population in State Prison, per 1,000 residents',
    subtitle = 'Inspired by the work of Prison Policy Initative, this map aggregates by census tract,
    positive block-level differences between the state-adjusted redistricting population versus 2020 census counts to show roughly how many people are missing out of their community due to being in state prison.',
    caption = 'Andy Arthur, 4/27/23.\nData Sources: LATFOR/US Census, methodology based on Prison Policy Initative.'
  ) +
  theme_void() +
  theme(
    text = element_text(family='Roboto Condensed', size=14),
    plot.title = element_text(face='bold', size=25, hjust=0.5),
    plot.subtitle = element_textbox( hjust=0.5, halign=0.5, width=0.95, margin=margin(0,0,30,0)),
    legend.position = c(-0.08,0.97),
    legend.justification = c(0,1),
    legend.key.size = unit(1.5, 'cm'),
    plot.caption = element_text(margin=margin(0,0,10,0)),
    plot.background = element_rect(fill='white', color='white')
  )

fn <- str_c('cap-reg-prison')
width <- 980
height <- 1200
dpi <- 130

ggsave(str_c('/tmp/',fn,'.jpg'), units='px', width=width, height=height, dpi=dpi)
ggsave(str_c('/tmp/',fn,'.svg'), units='px', width=width, height=height, dpi=dpi, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

# export block shapefil
op %>% 
  st_drop_geometry() %>%
  left_join(
    get_decennial(geography = "block", variables = "P1_001N", 
                  year = 2020, cache=T, geometry = T, state='ny'),
    .,
    join_by(GEOID)
    ) -> opbk

opbk %>%
  mutate(Pop = value.x, PP = Prisoners/value.x*1000) %>%
  write_sf('/tmp/prison.gpkg')
