library(tidyverse)
library(arcpullr)

gov <- readxl::read_xlsx('/tmp/2022_albany_county_races.xlsx', sheet = 1)

uri <- 'https://services8.arcgis.com/MVX6tbvWftyS3KBR/ArcGIS/rest/services/Albany_Couty_Election_Districts/FeatureServer/0'
ac <- get_spatial_layer(uri)

gov <- gov %>% mutate('ID' = as.numeric(`ED Code`), Ward = ifelse(Ward == 0, ' ', Ward))

acs <- tigris::county_subdivisions('ny','albany', cb=T)

ac %>% 
  inner_join(gov, by=c('Muni'='Municipality', 'WardNo'='Ward', 'DistNo'='ED')) %>%
  mutate(hoc=(`Kathy C. Hochul (DEM)`+`Kathy C. Hochul (WFP)`)/Ballot) %>%
  filter(hoc > .1) %>%
  ggplot() + geom_sf(aes(fill=hoc), linewidth=0.05, linetype='solid') +
  geom_sf(data=acs, fill=NA, linewidth=0.4) +
  ggredist::scale_fill_party_b(labels=scales::label_percent(), breaks=seq(0,1,.1), name='') +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 35pt">2022 Governor Race: <span style="color: darkblue">Kathy Hochul</span> ',
                      'vs. <span style="color: darkred">Lee Zeldin</span></span><br />',
                     'Albany County Board of Elections. ',
                     'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y.")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, size=12, face='bold', width=1),
    plot.background = element_rect(fill = "#FFFFFF", color="#FFFFFF"),
    plot.tag=ggtext::element_textbox(size=12, color='#555555', maxheight=0, halign = 1, hjust=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.76,0.03),
    strip.text = element_text(face='bold'),
    legend.text = element_text(size=10, margin=margin(r=4)),
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.direction = 'horizontal',
    legend.position = c(-0.01,.95),
    legend.justification = 'left'
  ) 
  
fn <- str_c('gov-race')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=110,  device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))




