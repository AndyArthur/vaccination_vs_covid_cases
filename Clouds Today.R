library(tidyverse)
library(terra)
library(sf)
library(ggtext)
library(stars)
library(lubridate)

rm(list=ls())
tmp1 <- tempfile()
tmp2 <- tempfile()
download.file('https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.ergrlake/VP.001-003/ds.sky.bin', tmp1)
download.file('https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.neast/VP.001-003/ds.sky.bin', tmp2)

r <- merge(rast(tmp1), rast(tmp2)) 
dt <- subset(r,1) 

st <- 'New York'
ny <- tigris::states() %>% filter(NAME==st)
ny2 <- project(vect(ny), crs(dt))

nyc <- tigris::counties(st, cb=T)
nycs <- tigris::county_subdivisions(st, cb=T)

s <- crop(dt, ny2, mask=T)
st_contour(st_as_stars(s), breaks = seq(0,100,5), contour_lines = F) -> contour

st_contour(st_as_stars(s), breaks = seq(0,100,1), contour_lines = T)  %>%
  #smoothr::smooth() %>%
  st_intersection(nyc %>% st_transform(crs(dt))) -> lines 

p <- contour %>% st_intersection(nyc %>% st_transform(crs(dt)))


ctymean$lab <- str_c(ctymean$NAME,'\n',round(ctymean$temp))

ggplot(p) + geom_sf(aes(fill=Min), size=0) +
  geom_sf(data=nycs, fill=NA, size=0.1) +
  #geom_sf(data=contour, fill=NA, color='white', size=0.1, alpha=0.4)+
  geom_sf(data=nyc, fill=NA, size=0.3) +
  coord_sf(expand=F, crs=3857) +
  scale_fill_gradient(low='lightblue',high='gray60') +
  theme_void() + 
  labs(title = str_c('<span style="color: gray; font-size: 62pt">High Temperature</span><br /><span style="font-size: 35pt"><em>',
                     format( now(),  format="%A,<br />%-B %-d, %Y") ,'</em></span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: National Digital Forecast Dataset'),
       fill = "")  +
  theme(
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=46, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(3.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
  ) 


fn <- str_c('high-temp-ny')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

