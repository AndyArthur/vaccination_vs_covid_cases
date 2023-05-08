library(tigris)
library(tidycensus)
library(sf)
library(ggtext)
library(tidyverse)
library(raster)
library(gstat)
rm(list=ls())

vars <- load_variables(2021, 'acs5')

get_acs(
  geography = "block group",
  state='ny',
  county = c('Albany','Saratoga','Schenectady','Rensselaer'),
  variables = "B01002A_001",
  year = 2021,
  survey = "acs5",
  geometry = T
) %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid() %>%
  st_transform(3857) -> income

incomeNA <- income %>% drop_na(estimate)

r <- raster(incomeNA, res=1000)
gs <- gstat(formula = estimate~1, locations = incomeNA)
nn <- interpolate(r, gs)

income <- cbind(income, est=exactextractr::exact_extract(nn, income, fun='median'))

income <- income %>% mutate(estimate = ifelse(is.na(estimate), est, estimate)) 

#income <- income %>% tigris::erase_water() %>% rmapshaper::ms_simplify()

nyco <- counties('ny') %>% filter(NAME %in% c('Albany','Saratoga','Schenectady','Rensselaer'))
nycos <- county_subdivisions('ny', county=c('Albany','Saratoga','Schenectady','Rensselaer'))

ggplot() + 
  geom_sf(data=income, aes(fill=estimate), linewidth=0) +
  scale_fill_viridis_b(option = 'D', labels=scales::label_dollar(), n.breaks=8) +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) + 
  geom_sf(data=nycos, fill=NA, color='white', linewidth=0.1) + 
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="color: navy; font-size: 48pt">Median Household Income</span> ',
                     '<br />in the Capital Region'),
       tag=paste('2021 American Community Survey, 5-yr Average with Missing Blockgroups Inverse Distance Weighted - Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),)  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple( face='bold', size=20, margin=margin(30, 0,30,0), halign=0.5, hjust=0.5),
    plot.background = element_rect(fill = "snow", color="snow"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0.5, color='#555555', halign = 0.5, valign = 0, margin=margin(15,5,5,5), width=1),
    plot.tag.position = 'bottom',
    legend.position = c(0.12, 0.7),
    legend.justification = 'right',
    legend.key.height = unit(2,'cm'),
    legend.key.width = unit(0.3,'cm'),
    legend.text = element_text(margin = margin(t = 30, unit = "pt")),
  ) 

fn <- str_c('median-hh-income')
width <- 920
height <- width*4/3

ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=width, height=height, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''),  width=width, height=height, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

