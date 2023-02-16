library(sf)
library(tigris)
library(tidyverse)
rm(list=ls())

nlcd <- terra::rast('~/Documents/GIS.Data/agriculture/nlcd2019_usa.tif')

decland <- arcpullr::get_spatial_layer('https://services6.arcgis.com/DZHaqZm9cxOD4CWM/arcgis/rest/services/NYS_DEC_Lands/FeatureServer/0')

catfp <- decland %>% filter(REGION %in% c(3,4), CATEGORY == 'FOREST PRESERVE') %>% st_make_valid() %>% group_by(FACILITY) %>% summarise()

cat_nlcd <- exactextractr::exact_extract(nlcd, catfp, 
                                        append_cols=T,
                                        summarize_df=T,
                                        fun=function(x) x %>% 
                                          group_by(nlcd_code = as.character(value)) %>% 
                                          summarise(area = sum(coverage_fraction)*30*30 %>%
                                                      units::set_units('m^2')))


cat_nlcd %>% mutate(nlcd_code = as.numeric(nlcd_code)) %>% 
  inner_join(FedData::pal_nlcd() %>% select(ID, Class), by=c('nlcd_code'='ID'))  %>% 
  pivot_wider(id_cols = FACILITY, names_from = Class, values_from = area) %>%
  mutate(total = rowSums(across(2:ncol(.)), na.rm=T),
         across(3:ncol(.)-1, ~./total),
         across(2:ncol(.), ~units::drop_units(.)),
         ) %>%
  select(-total) %>%
  pivot_longer(2:ncol(.)) %>%
  filter(grepl('Forest', name)) %>%
  left_join(catfp, ., by='FACILITY') %>%
  ggplot() + 
  geom_sf(aes(fill=value)) +
  facet_wrap(~name) +
  scale_fill_fermenter(palette = 'Greens', direction = 1, n.breaks=6, labels=scales::label_percent(), name='') +
  labs(
    title = 'Catskill Forest Preserve - Tree Cover',
    tag = 'Andy Arthur, 1/30/23. National Land Cover Dataset 2019, Percentage of Forest Preserve Area for Each Unit.'
  ) +
  theme_light() +
  theme(
    legend.position = c(1,1.12),
    legend.background = element_rect(fill=NA),
    legend.justification = 'right',
    legend.direction = 'horizontal',
    legend.key.height = unit(0.3,'cm'),
    legend.key.width = unit(2,'cm'),
  text=element_text(family='Noto Sans',size=12),
  plot.background = element_rect(fill = "mintcream", color=NA),
  plot.tag=element_text(size=10,hjust=0, color='#555555'),
  plot.margin = unit(c(1,1,1,1), 'lines'),
  plot.tag.position = c(0.03,-0.03),
  strip.text = element_text(face='bold',  margin=unit(c(5,0,5,0),'pt')),
)

fn <- str_c('catskill-fp-tree-cover')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=600, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=600, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


library(gt)

cat_nlcd %>% mutate(nlcd_code = as.numeric(nlcd_code)) %>% 
  inner_join(FedData::pal_nlcd() %>% select(ID, Class), by=c('nlcd_code'='ID'))  %>% 
  pivot_wider(id_cols = FACILITY, names_from = Class, values_from = area) %>%
  mutate(total = rowSums(across(2:ncol(.)), na.rm=T),
         across(3:ncol(.)-1, ~./total),
         across(2:ncol(.), ~units::drop_units(.)),
  ) %>%
  select(-total) %>%
  pivot_longer(2:ncol(.)) %>%
  filter(grepl('Forest', name)) %>%
  left_join(catfp, ., by='FACILITY') %>% 
  st_drop_geometry() %>%
  mutate(FACILITY = str_to_title(FACILITY), value = replace_na(value, 0)) %>%
  pivot_wider(names_from = 'name', values_from = value) %>%
  select(Unit = FACILITY, 2:4) %>%
  gt() %>%
  fmt_percent(2:4, decimals = 1) %>%
  cols_align('center') %>%
  data_color(2:4, colors = scales::col_numeric(palette = RColorBrewer::brewer.pal(8, 'Greens'), domain=c(0,1))) %>%
  tab_header('Forest Cover by Catskill Park Unit') %>%
  tab_footnote(html('Andy Arthur, 1/22/23.<br /><em>Data Source:</em> 2019 National Land Cover Dataset.')) %>%
  gtsave('/tmp/catlc.html')
