library(tidyverse)
library(tigris)
library(ggtext)
library(sf)
tmpf <- tempfile()

download.file('https://www.nysut.org/-/media/files/nysut/news/2023/202324executivebudgetstateaidprofile.xlsx', tmpf)

aid2023 <- readxl::read_excel(tmpf, sheet=2, skip=1)
aid2024 <- readxl::read_excel(tmpf, sheet=3, skip=1)

#nysd <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Schools/FeatureServer/18')
nysd <- read_sf('/home/andy/Documents/GIS.Data/education/SchDist_2019_v3.shp') %>% st_transform(3857) %>% rmapshaper::ms_simplify()

nyco <- counties(cb=T, resolution = '500k', 'ny')

nysd %>% 
  mutate(SDLCODE = ifelse(MUNICODE == '600534000000', 	
                          '300000', SDLCODE)) %>%
  left_join(aid2023, by=c('SDLCODE'='DABTB1')) %>% 
  left_join(aid2024, by=c('SDLCODE'='DABTA1')) %>% 
  transmute(
    tot.minus.build24 = 
      `AA(FA0189) 00 2023-24 TOTAL AID` -
      `J(FA0073) 00 2023-24 BUILDING AID` - 
      `K(FA0077) 00 2023-24 BUILDING REORG INCENTIVE AID` -
      `F(FA0013) 00 2023-24 CHARTER SCHOOL TRANSITIONAL`
      ,
    
    tot.minus.build23 = 
      `AA(FA0190) 00 2022-23 TOTAL AID` -
      `J(FA0074) 00 2022-23 BUILDING AID` -
      `K(FA0078) 00 2022-23 BUILDING REORG INCENTIVE AID` -
     `F(FA0014) 00 2022-23 CHARTER SCHOOL TRANSITIONAL`,
    chg = (tot.minus.build24-tot.minus.build23)/tot.minus.build23) %>%
  ggplot() + geom_sf(aes(fill=chg), linewidth=0.05, color='gray90') +
  geom_sf(data=nyco, fill=NA, color='white', linewidth=0.6) +
  scale_fill_viridis_b(n.breaks=8, labels=scales::label_percent(), name='', option = 'D') +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 45pt">NYS Total Education Aid</span> <em>excluding building, reorganization and charter aid</em><br /><br /><em>Change from <br />2022-23 Enacted to<br /> 2023-24 Proposed</em>'),
       y = "",
       x = "",
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: NYSED, NYSUT. nysut.org/-/media/files/nysut/news/2023/202324executivebudgetstateaidprofile.xlsx' ),
       fill = "") +
  theme(
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(2.3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=28, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 
fn <- str_c('total-no-building-aid')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

