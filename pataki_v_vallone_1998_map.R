library(tidyverse)
library(tigris)
library(sf)
library(ggtext)
library(cowplot)

# data sources:
# https://www2.census.gov/geo/tiger/TIGER2010/VTD/2000/
# the 36* VTDs files were combined into a single geopackage
# https://www.latfor.state.ny.us/data/2000elect/voteall.zip
# https://www.latfor.state.ny.us/data/2000elect/electhelp.zip

dict <- read_fwf(archive::archive_read('/home/andy/Downloads/electhelp.zip', 'vtddatadict.txt'), col_positions = fwf_widths(c(5,NA)))

er <- read_fwf(archive::archive_read('/home/andy/Downloads/voteall.zip', 'vote-all.txt'), 
         col_positions = fwf_widths(dict %>% mutate(val = lead(X1)-X1) %>% .$val, 
                                       col_names = dict$X2)
         )

er$vtd <- as.character(er$vtd)

nyco <- counties('ny',cb=T) %>% st_transform(3857)

vtd <- read_sf('~/Desktop/tl_2010_36_vtd00.gpkg') %>% st_transform(3857) %>% st_intersection(nyco)

vtd %>% inner_join(er, by=c('COUNTYFP00'='county fips','VTDST00'='vtd' )) %>% 
  mutate(percent = `Democratic...88`/(`Democratic...88`+`Republican...89`)) %>%
  ggplot() + 
  geom_sf(aes(fill=percent), size=0) +
  scale_fill_gradient2(low='red',high='blue', midpoint = 0.5, mid = 'white', na.value = 'white', 
                       breaks=seq(0,1,0.1), labels=scales::label_percent(), guide = "legend") +
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
  labs(title = str_c('<span style="font-size: 48pt; color: blue">Peter Vallone</span> vs <span style="font-size: 48pt; color: red">George Pataki</span> 1998 Gubernatoral Election'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: LATFOR, US Census Bureau, 2000 VTDs'),
       fill = "")  +
  theme(
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(0.5,'cm'),
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
  ) +
  guides(fill = guide_legend(byrow = TRUE, direction = 'horizontal', nrow=2) )


ggdraw() +
  draw_plot(nys) +
  draw_plot(nyc, x = 0.3, y = 0.04, width = 0.35, height = 0.26)

fn <- str_c('pataki-1998')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg, bg='white')

