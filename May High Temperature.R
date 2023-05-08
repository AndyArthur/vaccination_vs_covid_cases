library(tidyverse)
library(raster)
library(tigris)

rm(list=ls())
#https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf

tmpF <- tempfile()
tmpD <- tempdir()

download.file('https://ftp.prism.oregonstate.edu/normals_4km/tmax/PRISM_tmax_30yr_normal_4kmM4_05_bil.zip', tmpF)
unzip(tmpF, exdir = tmpD)

tmax <- raster(str_c(tmpD,'/',list.files(tmpD, pattern = '*.bil')[1]))

usco <- counties(cb=T,resolution = '20m') %>% filter(!STUSPS %in% c('HI','AK','PR'))
usst <- states(cb=T, resolution = '20m') %>% filter(!STUSPS %in% c('HI','AK','PR'))

usco %>% mutate(avghi = exactextractr::exact_extract((tmax * 9/5)+32, usco, 'median')) -> usco

usco <- rmapshaper::ms_simplify(usco)

ggplot(usco) + geom_sf(aes(fill=avghi), linewidth=0.1, color='gray40') + 
  geom_sf(data=usst, fill=NA, linewidth=0.6, color='gray40') +
  scale_fill_stepsn(name = "", colors = c("pink", "violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab",
                       na.value = "grey50", aesthetics = "fill", limits = c(-15,115), breaks=seq(-15,115,5)) +
  coord_sf(crs=5070, expand=F) +
  theme_void() +
  labs(title = str_c('<span style="font-size: 45pt">Average High Temperature<br />in May</span>'),
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),
                 '<br />Source: PRISM Group. Copyright ©2023, PRISM Climate Group, Oregon State University, prism.oregonstate.edu'),
  )  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold', size=12, width=0.7, margin=margin(0,0,10,0)),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555',  halign = 1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = 'bottom',
    legend.position = c(1,1.04),
    legend.justification = 'right',
    legend.direction = 'horizontal',
    legend.key.height = unit(0.4,'cm'),
    legend.key.width = unit(5,'cm'),
  ) 

fn <- str_c('may-high')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1350, units='px', dpi=130, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1350, units='px', dpi=130, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))

