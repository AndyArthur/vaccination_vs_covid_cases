library(tidyverse)
library(sf)
library(cowplot)
rm(list=ls())

# 2010
tdir <- tempdir()
unzip('/home/andy/Downloads/tl_2012_36_vtd10.zip', exdir = tdir)
vt10 <- read_csv(archive::archive_read('/home/andy/Downloads/REDIST_DATA.zip', 'vtd10vote.csv'))
nys <- states(cb=T) %>% filter(STUSPS == 'NY') %>% st_transform(3857)
vtd10 <- read_sf(str_c(tdir,'/tl_2012_36_vtd10.shp')) %>% st_transform(3857) %>% st_intersection(nys)
vt10$NOPAD_VTDID <- as.character(vt10$NOPAD_VTDID)
vtd10 %>% inner_join(vt10, by=c('GEOID10'="NOPAD_VTDID")) -> vtd

vtd10g <- vtd %>% transmute(GEOID10, gov10 = DEM10G21/(DEM10G21+REP10G21))


# 2018
vt20 <- read_csv('~/2020vote_vtd.csv')
vt20$GEOID <- as.character(vt20$GEOID)
vtd <- voting_districts('ny', cb=T) %>%
  inner_join(vt20, by=c('GEOID20'='GEOID')) %>%
  st_transform('epsg:3857')

vtd20g <- vtd %>% transmute(GEOID20, gov18 = DEM18G21/(DEM18G21+REP18G21))

pop <- get_decennial(geography = "block", state = 'ny', variables = "P1_001N", 
                    year = 2020, cache=TRUE, geometry = T)

pop <- st_transform(pop, 3857)

vtd10g <- vtd10g %>% st_cast('MULTIPOLYGON')

vtdi <- interpolate_pw(
  vtd10g,
  vtd20g,
  extensive = F,
  weights = pop,
  weight_column = 'value'
)
  
  
chvtd <- cbind(gov10=vtdi$gov10, vtd20g)


nyco <- counties(cb=T, 'ny', resolution = '500k') %>% st_transform(3857) 


ggplot(chvtd) + 
  geom_sf(aes(fill=gov10-gov18), size=0) + 
  scale_fill_gradient2(low='blue',high='red', midpoint = 0, mid = 'white', na.value = 'white', 
  breaks=seq(-0.5,0.8,.2), limits=c(-0.5,0.5), labels=scales::label_percent()) +
  geom_sf(data=nyco, fill=NA) +
  theme_void() -> main

# NYC Subset
bbox <- nyco %>% filter(NAME %in% c('Richmond','Bronx','Queens')) %>% st_bbox()
nyc <- main + coord_sf(xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]), expand=T, crs=3857) +
  theme(panel.border = element_rect(fill=NA, size=0.5), legend.position = 'none',
        plot.title=element_text(family='Lato',size=12, margin=margin(b=3))) +
  labs(title='New York City')  

nys <- main + 
  coord_sf(expand=F, crs=3857) +
  labs(title = str_c('<span style="font-size: 48pt; color: gray10">Partisan</span> <span style="font-size: 48pt; color: blue">Shift</span> Between  <span style="font-size: 48pt; color: red">2010 and 2018</span> Gubernatoral Elections'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: LATFOR, US Census Bureau, 2010 VTDs were Population Weighted Interpolated to 2020 VTDs.'),
       fill = "")  +
  theme(
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.position = c(0.00,0.17),
    legend.spacing.y = unit(0.5, 'cm'),
    legend.direction = 'horizontal',
    legend.justification = 0,
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(25,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) 


ggdraw() +
  draw_plot(nys) +
  draw_plot(nyc, x = 0.3, y = 0.04, width = 0.35, height = 0.26)

fn <- str_c('shift-from-2010')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg, bg='white')
#ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg, bg='white')
#system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
#unlink(str_c('~/Desktop/',fn,'.svg'))






