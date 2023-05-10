library(tidyverse)
library(ggtext)
dams <- arcpullr::get_spatial_layer('https://ags03.sec.usace.army.mil/server/rest/services/Dams_Public/FeatureServer/0', where = "STATE='New York'")
nyco <- tigris::counties('ny',cb=T)

dams %>%
  filter(CONDITION_ASSESSMENT == 'Poor', HAZARD_POTENTIAL == 'High') %>%
  ggplot() + 
  geom_sf(data=nyco) +
  geom_sf(aes(color=DAM_HEIGHT, size=SURFACE_AREA)) +
  scale_size(labels=scales::label_comma(), name='Area Impounded (Acres)', range = c(1,8), trans = 'sqrt') +
  scale_color_fermenter(palette = 'YlOrBr', direction = -1, name='Dam Height (Feet)') + 
  theme_void() +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 55pt">High Hazard Potential Dams in Poor Condition</span><br /><em style="font-size: 20px">National Inventory of Dams</em>'),
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: US Army Corps of Engineers. https://ags03.sec.usace.army.mil/server/rest/services/Dams_Public/FeatureServer/0' ),
       fill = "") +
  theme(
    legend.key.height = unit(0.6,'cm'),
    legend.key.width = unit(2.3,'cm'),
    legend.position = c(0,0.1),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, lineheight = 0.9, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal',
    legend.justification = c(0.0,0),
  ) +
  guides(
    size = guide_legend(title.position = 'top', label.position = 'bottom', title.theme = element_text(margin = margin(20), size = 14, face='bold')),
    color = guide_legend(title.position = 'top', override.aes = list(size=5), label.position = 'bottom', title.theme = element_text(size = 14, face='bold')) 
  )

fn <- str_c('high-hazard')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=110,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

