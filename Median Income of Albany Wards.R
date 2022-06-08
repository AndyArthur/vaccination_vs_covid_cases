library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)

income<- get_acs(
  geography = "tract",
  state='ny',
  county='Albany',
  variables = "B19013_001",
  year = 2020,
  survey = "acs5",
  geometry = T
)

incomebak <- income

wards <- read_sf('/tmp/Albany Wards.gpkg') %>%
  st_transform(3857) %>%
  mutate(ward_area = st_area(.),
         Ward = as.numeric(Ward)) %>%
  dplyr::select(Ward, ward_area)


income <- income %>% 
  st_transform(3857)

income %>% 
  st_intersection(wards) %>%
  mutate(percent_of_ward = (st_area(.) / ward_area) %>% units::drop_units() ) %>%
  st_drop_geometry() %>%
  group_by(Ward) %>%
  summarise(estimate = weighted.mean(estimate, percent_of_ward, na.rm=T)) %>%
  inner_join(wards, ., by=c('Ward'))  %>%
  ggplot() + 
    geom_sf(aes(fill=estimate), size=0.4, color='black') +
    ggsflabel::geom_sf_label_repel(aes(label=str_c('Ward ',Ward,'\n',scales::dollar(estimate, accuracy = 1)), geometry=geom),
                                 size=2.5, point.size = NA, fill='white', box.padding = 0.2, label.padding = unit(0.2, "lines"), family='Noto Sans', max.overlaps=NA ) +
  scale_fill_viridis_c(option='D', label=scales::dollar_format()) +
  labs(title = '<span style="color: darkgreen">Median Household Income</span> by Albany City Ward',
       subtitle = 'Taking the area-weighted mean of Census Tract\'s Median Household Income for each city ward.',
       y = "",
       x = "",
       caption='2020 ACS 5-Yr Average, Tract Area-Weighted Means',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'top',
  ) +
  coord_sf(expand=F)  

fn <- str_c('albany-city-income')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

