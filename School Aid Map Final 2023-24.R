library(tidyverse)
library(tigris)
tmpf <- tempfile()

download.file('https://www.nysut.org/-/media/files/nysut/news/2023/202324-enacted-budget-school-aid-profile.xlsx', tmpf)

aid2023 <- readxl::read_excel(tmpf, sheet=2, skip=1) %>%
  mutate(across(3:ncol(.), as.numeric)) %>%
  janitor::clean_names()

aid2024 <- readxl::read_excel(tmpf, sheet=3, skip=1) %>%
  mutate(across(3:ncol(.), as.numeric)) %>%
  janitor::clean_names()

nysd <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Schools/FeatureServer/18') %>% 
  rmapshaper::ms_simplify()

nyco <- counties(cb=T, resolution = '500k', 'ny')

nysd %>% 
  mutate(SDLCODE = ifelse(MUNICODE == '600534000000', 	
                          '300000', SDLCODE)) %>%
  inner_join(aid2023, by=c('SDLCODE'='dbsab1')) %>% 
  inner_join(aid2024, by=c('SDLCODE'='dbsaa1')) %>% 
  mutate(chg = (e_fa0197_00_2023_24_foundation_aid-e_fa0198_00_2022_23_foundation_aid) / 
           e_fa0198_00_2022_23_foundation_aid) %>%
  ggplot() + geom_sf(aes(fill=chg), linewidth=0.05, color='gray90') +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) +
  scale_fill_viridis_b(option = 'A', n.breaks=8, labels=scales::label_percent(), name='') +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 55pt">NYS Education Foundation Aid</span><br /><br /><em>Change from <br />2022-23 Enacted to<br /> 2023-24 Enacted</em>'),
       y = "",
       x = "",
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: NYSUT. https://www.nysut.org/-/media/files/nysut/news/2023/202324-enacted-budget-school-aid-profile.xlsx' ),
       fill = "") +
  theme(
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(2.3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=30, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 
fn <- str_c('foundation-aid')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=110,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


nysd %>% 
  mutate(SDLCODE = ifelse(MUNICODE == '600534000000', 	
                          '300000', SDLCODE)) %>%
  inner_join(aid2023, by=c('SDLCODE'='dbsab1')) %>% 
  inner_join(aid2024, by=c('SDLCODE'='dbsaa1')) %>% 
  mutate(
    tot.minus.bld.24 = aa_fa0189_00_2023_24_total_aid-j_fa0073_00_2023_24_building_aid-
      k_fa0077_00_2023_24_building_reorg_incentive_aid-f_fa0013_00_2023_24_charter_school_transitional,
    tot.minus.bld.23 = aa_fa0190_00_2022_23_total_aid-j_fa0074_00_2022_23_building_aid-
      k_fa0078_00_2022_23_building_reorg_incentive_aid-f_fa0014_00_2022_23_charter_school_transitional,
    chg = (tot.minus.bld.24-tot.minus.bld.23)/tot.minus.bld.23
           ) %>%
  ggplot() + geom_sf(aes(fill=chg), linewidth=0.05, color='gray90') +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) +
  scale_fill_viridis_b(option = 'B', n.breaks=8, labels=scales::label_percent(), name='') +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 55pt">NYS Total Education Aid</span><br /><em style="font-size: 20px">Excluding building, building reorganization and charter school set-aside aid.</em><br /><br /><em>Change from <br />2022-23 Enacted to<br /> 2023-24 Enacted</em>'),
       y = "",
       x = "",
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: NYSUT. https://www.nysut.org/-/media/files/nysut/news/2023/202324-enacted-budget-school-aid-profile.xlsx' ),
       fill = "") +
  theme(
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(2.3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, lineheight = 0.9, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 
fn <- str_c('total-aid')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=110,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
