library(tidyverse)
library(tidycensus)
library(tigris)
library(ggtext)

#var <- load_variables(2021, 'acs5')

medicaid <- get_acs('county', 
        variables = str_c('B27010_0',c('07',13,23,29,39,46,62)),
        summary_var = 'B27010_001',
        state = 'ny',
)

nytract <- counties(year=2021, 'ny', cb=T)

clr <- viridis::viridis(10)
  
medicaid %>%
  group_by(GEOID) %>%
  summarise(percent_medicaid = sum(estimate)/first(summary_est)) %>%
  inner_join(nytract, .) %>%
  ggplot() +
  geom_sf(aes(fill=percent_medicaid), linewidth=.5, color='white') +
  scale_fill_viridis_b(labels=scales::label_percent(), name="", breaks=seq(0,1,0.05)) +
  coord_sf(expand=F) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 60pt; color: ',clr[3],'">Medicaid Health Insurance</span>'),
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: 2021 ACS 5-yr, Table 27010'),
       fill = "") +
  theme(
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=20, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width=0.4, color='gray30'),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 

fn <- str_c('medicaid-health')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))

