library(tidyverse)
library(readxl)
library(sf)
library(ggtext)
rm(list=ls())
fs <- read_xls('~/Downloads/2021-munis-all-data-worksheet.xls', sheet = 1, skip = 5)

fs <- fs %>% mutate(NAMELSAD = str_c(Name,' ',str_to_lower(Class)),
                    NAMELSADCO = str_c(County,' County'))

nycos <- tigris::county_subdivisions('ny', cb=T) %>% rmapshaper::ms::ms_simplify()
nyco <- tigris::counties('ny', cb=T)


stress <- nycos %>% full_join(fs, by=c('NAMELSAD', 'NAMELSADCO')) %>%
  filter(`Type of Stress` != 'No Designation' &
           `Type of Stress` != 'Not filed'  &
           `Type of Stress` != 'N/A - Coterminous'
           )

nycos %>% full_join(fs, by=c('NAMELSAD', 'NAMELSADCO')) %>% 
  ggplot() + geom_sf(aes(fill=`Fiscal Score`), size=0.1) +
  scale_fill_viridis_b(breaks=seq(0,100,10), na.value = 'gray90', name='') +
  geom_sf(data=nyco, fill=NA) +
  ggsflabel::geom_sf_label_repel(data=stress, aes(label=str_c(NAMELSAD,'\n',`Type of Stress`)), point.padding=20, label.r=unit(0,'pt'), size=2.3) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 38pt; color: darkgreen">Municipal Fiscal Distress Rating</span><br /><br />2021 NYS Comptroller</em>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),
                 '\nSource: ',
                 '2021 Local Government Fiscal Distress, NYS Comptroller\'s Office\n',
                 'https://www.osc.state.ny.us/local-government/fiscal-monitoring/lists ',
                 'Note: Areas in gray did not file required paperwork with the Comptroller\'s office.'),
       fill = "")  +
  theme(
    legend.key.height = unit(1.5,'cm'),
    legend.key.width = unit(1,'cm'),
    legend.position = c(0.27,0.17),
    legend.spacing.y = unit(0.5, 'cm'),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=25, margin=unit(c(5,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01)
  ) +
  guides(fill = guide_legend(byrow = TRUE, direction = 'horizontal') )

fn <- str_c('fiscal-distress')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))


                    