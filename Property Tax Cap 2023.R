library(tidyverse)
library(tigris)
library(sf)
library(ggtext)

rm(list=ls())

PTCR <- tempfile()
download.file('https://www.p12.nysed.gov/mgtserv/propertytax/docs/PTRC_23-24_FINAL.xlsx', PTCR)
PTCR <- readxl::read_excel(PTCR)


nysd <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NYS_Schools/FeatureServer/18') %>%
  rmapshaper::ms_simplify()

PTCR %>%
  mutate(
    `Cap.Status` = PTCR$`(w/o Exc.) Tax Levy vs. Tax Levy Limit 2023-24`+PTCR$`Permissible Exclusions 2023-24`<0) %>%
  left_join(nysd, ., join_by(SDLCODE == `BEDS Code`)) %>%
  ggplot() + 
  geom_sf(aes(fill=Cap.Status), size=0.1) +
  scale_fill_hue(direction = -1, labels=c('Within Tax Cap','Override')) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="color: ',scales::hue_pal()(2)[1],'; font-size: 45pt">School Districts Overriding the Tax Cap</span>'),
       tag = paste(
         'Andy Arthur,', format(Sys.Date(), format="%m/%d/%y"), '<br /><br />',
         'Data Source: NYS Property Tax Report Card. p12.nysed.gov/mgtserv/propertytax/'
       ),
       fill = "")  +
  theme(
    text= element_text(family='chivo',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=56, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=16,hjust=0, color='#555555', width=0.7, valign=0, vjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = c(0.93, 0.6),
    legend.key.height = unit(2,'cm')
  ) 

fn <- str_c('school-tax-cap-overide')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1600, height=1080, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1600, height=1080, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

library(gt)

PTCR %>%
  mutate(
    `Cap.Status` = PTCR$`(w/o Exc.) Tax Levy vs. Tax Levy Limit 2023-24`+PTCR$`Permissible Exclusions 2023-24`<0) %>%
  filter(Cap.Status) %>%
  transmute(
    `District Name` = str_to_title(`District Name`),
                                   `Total Proposed Spending 2022-23`, 
                                   `Total Proposed Spending 2023-24`,
                                   `Spending Percent Change`,
                                   `Proposed Tax Levy Percent Change`,
    `Tax Levy Above Permissible Exclusions` = (`(w/o Exc.) Tax Levy vs. Tax Levy Limit 2023-24` + `Permissible Exclusions 2023-24`)*-1
    ) %>%
  arrange(`District Name`) %>%
  gt() %>%
  fmt_currency(c(2:3,6), suffixing = T) %>%
  fmt_percent(4:5, scale_values = F) %>%
  tab_options(table.width=800) %>%
  gtExtras::gt_theme_538() %>%
  tab_header('School Districts Overriding the Tax Cap') %>%
  tab_footnote(html('Andy Arthur, 5/8/23.<div style="float: right"><em>Data Source:</em>
          NYS Property Tax Report Card. p12.nysed.gov/mgtserv/propertytax/')) %>%
  #gtsave(str_c('/tmp/taxlevy.html')) 
  gtsave('/tmp/taxlevy.png', expand=c(5,5,15,5), vwidth=1920)
}

    