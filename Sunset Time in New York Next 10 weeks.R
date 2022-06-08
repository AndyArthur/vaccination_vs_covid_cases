imi
nyco <- counties('ny', cb=T)
nycosub <- county_subdivisions('ny', cb=T) 
nycosub$tz <- nycosub %>% st_centroid() %>% st_coordinates() %>% find_tz()
nycosub <- cbind(nycosub %>% st_centroid() %>% st_coordinates(), nycosub)


for (day in seq(as.Date('2022-06-06'), as.Date('2023-6-6'), 7)) {
  day <- as.Date(day, origin = as.Date('1970-01-01'))
  nytime <- cbind(nycosub, getSunlightTimes(data=data.frame(date=day, lat=nycosub$Y, lon=nycosub$X, tz=nycosub$tz)))
  nytime <- nytime %>% st_set_geometry('geometry')
  
  ggplot(nytime) + 
    geom_sf(aes(fill=sunrise), size=0) +
    geom_sf(data=nyco, fill=NA) +
    coord_sf(crs=3857, expand=F) + 
    scale_fill_datetime(low='yellow', high='darkred', date_labels='%-I:%M%p', timezone=first(nycosub$tz),
                        date_breaks = '5 min',
                        guide =  guide_colourbar(ticks = T, ticks.linewidth = 2)
                        ) +
    theme_void() +
    labs(title = str_c('<span style="color: darkred">Sunrise Time</span> for <span style="color: orange">', format(day,  format="%-B %-d"),'</span>'),
         y = "",
         x = "",
         caption='R/suncalc',
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
      legend.key.height = unit(0.3,'cm'),
      legend.key.width = unit(5,'cm'),
      legend.position = 'top', 
      ) 

  fn <- str_c(day)
  ggsave(paste('/tmp/sunrise',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
  #ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
  #system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
  
}






