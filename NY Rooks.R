library(tigris)
library(ggtext)
library(tidyverse)
rm(list=ls())
nyrook <- counties('ny') %>% vect() %>% adjacent("rook", pairs=F) %>% rowSums() %>% cbind(counties('ny'), rook=as.factor(.))

ggplot(nyrook) + geom_sf(aes(fill=rook)) + 
  scale_fill_brewer(palette = 'Spectral',direction=-1) +
  ggsflabel::geom_sf_label_repel(aes(label=rook),
                                 size=2.5, point.size = NA, fill='white', box.padding = 0.2, label.padding = unit(0.2, "lines"), family='Noto Sans', max.overlaps=NA ) +
  labs(title = '<span style="color: darkgreen">Rook Neighbors</span> New York Counties',
       subtitle = 'Calculated the conventional way, ignoring the bridge rule in New York State.',
       y = "",
       x = "",
       caption='R terra, adjacent("rook", pairs=F) %>% rowSums()',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'top',
  ) +
  coord_sf(expand=F)  

fn <- str_c('ny-rook')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


