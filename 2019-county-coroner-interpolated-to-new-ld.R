library(tidyverse)
library(tidycensus)
library(sf)
rm(list=ls())
ac19 <- readxl::read_excel('~/Desktop/2019_albany_county_races.xlsx', sheet = 7) %>% 
  mutate(`Ward` = as.numeric(`Ward`),
         `ED` = as.numeric(`ED`))

acdist <- read_sf('/home/andy/Desktop/2017_albany_county_election_districts.gpkg') %>%
  mutate(TOWN = str_replace(TOWN, 'Rennselaerville', 'Rensselaerville'))

acdist <- acdist %>% inner_join(ac19, c('TOWN'='Municipality',
                                        'WARD'='Ward',
                                        'ED'='ED'))

# use VAP for interpolation
tb <- get_decennial('block', state='ny', county='Albany', variables = 'P3_001N', year=2020, geometry = T)

acdist <- acdist %>% st_transform(26918) 
tb <- tb %>% st_transform(26918)

ac19b <- interpolate_pw(
    acdist, 
    tb,
    extensive = T,
    weights = tb,
    weight_column = 'value',
    to_id='GEOID'
)

library(readxl)
path <- '/home/andy/Desktop/Attachment A-2 - ACRC Final District Blocks.xlsx'
ldb <- path %>%
  excel_sheets() %>%
  map_df(~read_excel(path, sheet=.)) 

library(gt)
library(gtExtras)
ac19b %>% 
  inner_join(ldb, by=c('GEOID'='BLOCK')) %>%
  st_drop_geometry() %>%
  group_by(DISTRICT) %>%
  summarise(`Antonio Sturges (DEM)` = sum(`Antonio Sturges (DEM)`), `William B. Keal (REP)` = sum(`William B. Keal (REP)`)) %>%
  #mutate(across(2:3, ~round(.))) %>%
  mutate(`Dem Margin` = `Antonio Sturges (DEM)` - `William B. Keal (REP)`, 
         `Dem. Percent` = `Antonio Sturges (DEM)`/(`Antonio Sturges (DEM)`+`William B. Keal (REP)`)) %>%
  select('Proposed LD' = DISTRICT, everything()) %>%
  filter(`Proposed LD` >20 ) %>%
  gt() %>%
  tab_header(
    title = html('<b>Proposed County Legislative Districts (12/9)</b><br />2019 County Coroner Race PW-VAP Interpolated')
  ) %>% 
  tab_footnote(html('Andy Arthur, 12/26/22. Estimates created in R using block-level voting-age population interpolation (ERSI method) of 2019 Election Results/Districts.')) %>%
  tab_style(locations = list(cells_body(), cells_column_labels()), cell_text(size = px(14))) %>%
  fmt_integer(2:4) %>%
  fmt_percent(last_col(), scale_values = T, decimals = 0)  %>%
  gt_color_rows(last_col(), domain= 0:1, palette = c('red','white','darkblue')) %>%
gtsave('/tmp/coroner2039.html')

library(ggtext)


ac19b %>% 
  inner_join(ldb, by=c('GEOID'='BLOCK')) %>%
  group_by(DISTRICT) %>%
  summarise(`Antonio Sturges (DEM)` = sum(`Antonio Sturges (DEM)`), `William B. Keal (REP)` = sum(`William B. Keal (REP)`)) %>%
  mutate(across(2:3, ~round(.))) %>%
  mutate(dem_margin = `Antonio Sturges (DEM)` - `William B. Keal (REP)`, 
         dem_percent = `Antonio Sturges (DEM)`/(`Antonio Sturges (DEM)`+`William B. Keal (REP)`)) %>%
  ggplot() + geom_sf(aes(fill=dem_percent)) +
  ggredist::scale_fill_party_b() +
  ggsflabel::geom_sf_text_repel(aes(label=DISTRICT), size=2.5, point.size=NA) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<b style="font-size: 40pt">Proposed County Legislative Districts (12/9/22)</b><br />2019 County Coroners Race PW-VAP Interpolated'),
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"), 
         '. <br />Estimates created in R using block-level voting-age population<br /> interpolation (ERSI method) of 2019 Election Results/Districts.'),
       fill = "") +
  theme(
    legend.key.height = unit(2,'cm'),
    legend.key.width = unit(0.3,'cm'),
    legend.position = 'right',
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, size=20,  color='gray30', margin=unit(c(5,0,5,0),'pt')),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=element_textbox(size=10, color='#555555', halign=1, hjust=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.01),
    legend.direction = 'vertical'
  ) 

fn <- str_c('coroner-ld-2022')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

nycos <- tigris::county_subdivisions('ny','albany', cb=T)

acdist %>%
  mutate(dem_margin = `Antonio Sturges (DEM)` - `William B. Keal (REP)`, 
         dem_percent = `Antonio Sturges (DEM)`/(`Antonio Sturges (DEM)`+`William B. Keal (REP)`)) %>%
  ggplot() + geom_sf(aes(fill=dem_percent)) +
  ggredist::scale_fill_party_b() +
  geom_sf(data=nycos, linewidth=1, fill=NA)+
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<b style="font-size: 40pt">2019 County Coroner Race by ED'),
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"), 
         '. <br />2019 Election Results/Districts.'),
       fill = "") +
  theme(
    legend.key.height = unit(2,'cm'),
    legend.key.width = unit(0.3,'cm'),
    legend.position = 'right',
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, size=20,  color='gray30', margin=unit(c(5,0,5,0),'pt')),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=element_textbox(size=10, color='#555555', halign=1, hjust=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.01),
    legend.direction = 'vertical'
  ) 

fn <- str_c('coroner-2019')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))




