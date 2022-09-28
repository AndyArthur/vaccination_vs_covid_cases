library(tidyverse)
library(tigris)
rm(list=ls())
re <- read_csv('https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_County.csv', col_types = c('c','c'))

re$county_fips <- str_pad(re$county_fips,5,side='left', pad='0')
hh <- get_acs(geography = 'county', year = 2020, survey = 'acs5', variables = 'B19049_001', state='ny', geometry = T) 

hh %>% inner_join(re, by=c('GEOID'='county_fips')) %>%
  mutate(years2buy = median_listing_price/(estimate*.30)) %>%
  ggplot() + geom_sf(aes(fill=years2buy)) +
  ggsflabel::geom_sf_text_repel(aes(label=floor(years2buy)), point.size=NA, size=3.2, alpha=0.9, bg.color = "white", bg.r = 0.25 ) +
  scale_fill_fermenter(name="", palette = 'YlOrRd', direction = 1, breaks=seq(0,80,10)) + 
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 38pt; color: darkred">How Many Years To Buy a House</span><br /><br />At 30% of Median Household Income<br><br /><em>August 2022 Listings</em>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: RDC Income Core Metrics - Listing Price / Median HH Income (2020 5-Yr ACS) / 30.\nThis is listing price and does not include financing or other costs of purchase.'),
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
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) +
  guides(fill = guide_legend(byrow = TRUE, direction = 'horizontal') )



fn <- str_c('afford-house')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))

