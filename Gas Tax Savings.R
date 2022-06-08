library(tidyverse)

gas <- read_csv('Desktop/Gas Tax Holiday.csv')

nyco <- counties('ny',cb=T)

nyco %>% 
  inner_join(gas, by=c('NAME'='County')) %>%
  ggplot() + 
  geom_sf(aes(fill=Savings*15), color='white') +
  ggsflabel::geom_sf_label_repel(aes(label=str_c(scales::dollar(Savings*15, accuracy = 0.01)), geometry=geometry), segment.color='white',
                                 size=2.5, point.size = NA, fill='white', box.padding = 0.2, label.padding = unit(0.2, "lines"), family='Noto Sans', max.overlaps=NA ) +
  scale_fill_distiller(palette = 'GnBu', direction=-1, label=scales::dollar_format()) +
  labs(title = str_c('How Much You <span style="color: darkgreen">Save with the Gas Tax Holiday</span> on 15 Gallons' ),
       subtitle = 'Some counties have reduced taxes more then others, but generally the tax holiday has modest impacts on cost of fueling up when 15 gallons costs $75.',
       y = "",
       x = "",
       caption='https://tinyurl.com/ny-comptroller-gas',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=30, margin=unit(c(5,0,3,0),'pt'), lineheight = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, face='italic', margin=unit(c(0,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'top',
  ) +
  coord_sf(expand=F)  

fn <- str_c('gas-holiday-tax')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

