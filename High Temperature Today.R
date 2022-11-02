library(tidyverse)
library(terra)
library(sf)
library(ggtext)
library(stars)
library(lubridate)

rm(list=ls())
tmp1 <- tempfile()
tmp2 <- tempfile()
download.file('https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.ergrlake/VP.001-003/ds.maxt.bin', tmp1)
download.file('https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.neast/VP.001-003/ds.maxt.bin', tmp2)

r <- merge(rast(tmp1), rast(tmp2)) 
dt <- subset(r,1) * 9/5+32

st <- 'New York'
ny <- tigris::states() %>% filter(NAME==st)
ny2 <- project(vect(ny), crs(dt))

nyc <- tigris::counties(st, cb=T)
nycs <- tigris::county_subdivisions(st, cb=T)

s <- crop(dt, ny2, mask=T)
st_contour(st_as_stars(s), breaks = seq(-40,110,5), contour_lines = F) -> contour

st_contour(st_as_stars(s), breaks = seq(-40,110,1), contour_lines = T)  %>%
  #smoothr::smooth() %>%
  st_intersection(nyc %>% st_transform(crs(dt))) -> lines 

p <- contour %>% st_intersection(nyc %>% st_transform(crs(dt)))


ctymean <- nycs %>% filter(NAMELSAD %in% c('Albany city', 'Plattsburgh city', 
                                           'Blenheim town', 'Hector town', 
                                           'Inlet town', 'Stratford town',
                                           'Spectulator town', 'Syracuse city',
                                           'Utica city', 'Lowville town',
                                           'Rochester city', 'Hunter town',
                                           'Poughkeepsie city', 'Lake Pleasant town',
                                           'Buffalo city', 'Salamanca city',
                                           'Riverhead town', 'East Otto town',
                                           'Brookfield town', 'Minerva town',
                                           'Delhi town', 'Brasher town',
                                           'Franklin town', 'Nelson town',
                                           'Johnsburg town', 'Greene town',
                                           'Pharsalia town', 'Hadley town',
                                           'Alfred town', 'Grafton town', 'Westerlo town',
                                           'Rockland town', 'Forestport town',
                                           'Redfield town', 'Harrisville town', 
                                           'Beacon city',
                                           'Manhattan borough','Riverhead city'))


ctymean <- cbind(
  temp=exactextractr::exact_extract(s, ctymean, fun=c('mean')),
  ctymean) %>% st_set_geometry('geometry')

ctymean$lab <- str_c(ctymean$NAME,'\n',round(ctymean$temp))


colramp <- scales::gradient_n_pal(c("#DC143C", "pink", "violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab")
colT <- colramp((mean(c(p$Min, p$Max))--35) / (115--35))

ggplot(p) + geom_sf(aes(fill=Min), size=0) +
  geom_sf(data=nycs, fill=NA, size=0.1) +
  #geom_sf(data=contour, fill=NA, color='white', size=0.1, alpha=0.4)+
  geom_sf(data=nyc, fill=NA, size=0.3) +
  ggsflabel::geom_sf_text_repel(data=ctymean, aes(label=lab), 
                                size=4, point.size = NA, box.padding = unit(0.2, "lines"),
                                bg.r=0.2, bg.color='#ffffffcc', fontface='bold', lineheight=0.8,
                                min.segment.length =0, family='Lato') +
  scale_fill_gradientn(name = "", colors = c("#DC143C", "pink", "violet", "purple", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill", limits = c(-35,115), seq(-30,115,10)) +
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: ',colT,'; font-size: 62pt">High Temperature</span><br /><span style="font-size: 35pt"><em>',
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

