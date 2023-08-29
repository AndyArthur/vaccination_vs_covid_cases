library(tigris)
library(tidycensus)
library(sf)
library(tidyverse)
library(gstat)
library(ggtext)
library(raster)

get_acs(
  geography = "tract",
  state=c('dc','va','md','wv'),
  variables = "B19013_001",
  year = 2021,
  survey = "acs5",
  geometry = T
) -> income

income %>% st_transform(3857) %>% st_make_valid() %>%
  filter(st_dimension(geometry) > 1) -> 
  income

incomeNA <- income %>% drop_na(estimate)

r <- raster(incomeNA, res=1000)
gs <- gstat(formula = estimate~1, locations = incomeNA)
nn <- interpolate(r, gs)

income <- cbind(income, est=exactextractr::exact_extract(nn, income, fun='median'))
income <- income %>% mutate(estimate = ifelse(is.na(estimate), est, estimate)) 
income <- income %>% rmapshaper::ms_simplify()

sts <- states(cb=T) %>% filter(STUSPS %in% c('DC','VA','MD','WV'))
ctys <- counties(cb=T, state=c('DC','VA','MD','WV'))

richmond <- ctys %>% filter(GEOID == 51760)


ggplot(income) + geom_sf(aes(fill=estimate), linewidth=0) + 
  geom_sf(data=ctys, fill=NA, color='white', linewidth=0.1) +
  geom_sf(data=richmond, fill=NA, color='#ffffd9', linewidth=1) +
  geom_sf(data=sts, fill=NA, color='white', linewidth=0.5) +
  scale_fill_fermenter(palette = 'YlGnBu', n.breaks=8, labels=scales::label_dollar(), name='Median Household Income - Tract') +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 30pt; color: #081d58">The <span style="font-size: 35pt; color: #7fcdbb">Rich Men</span> ',
                     '<span style="font-size: 35pt; color: #1d91c0">North</span> of ',
                     '<span style="font-size: 35pt; color: #253494">Richmond</span></span><br />',
                     'A look at the Median Household Income by Census Tract in the states surrounding District of Columbia.'),
       y = "",
       x = "",
       tag=paste('2021 American Community Survey (ACS) 5-year estimates with missing data inverse distance interpolated. | ',
                 'Andy Arthur | ', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=12, width=0.5, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=0.5, color='#555555', margin=margin(t=10), halign = 0.5),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = 'bottom',
    legend.position = c(0.8,1.04),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(3,'cm'),
  ) +
  guides(fill=guide_colourbar(title.position='top', title.theme = element_text(hjust=0.5, face = 'bold')))

fn <- str_c('rich-men-north-of-richmond')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=130)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

source('upload-svg.R', local=T)
