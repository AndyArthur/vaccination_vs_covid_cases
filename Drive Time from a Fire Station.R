library(arcpullr)
library(tidyverse)
library(tigris)
library(units)
library(ggtext)
library(mapboxapi)

albbuf <- county_subdivisions('ny') %>% filter(NAME == 'Albany') %>% st_transform(4326)

albfire <- arcpullr::get_layer_by_poly('https://services1.arcgis.com/Hp6G80Pky0om7QvQ/ArcGIS/rest/services/Fire_Station/FeatureServer/0', 
                           albbuf )
albfire <- albfire %>% st_transform(26918)

alb <- county_subdivisions('ny') %>% filter(NAME == 'Albany')

blocks('ny', 'albany') %>% st_intersection(alb) -> albb

times <- mb_matrix(albb, albfire)

albb$min_time <- apply(times, 1, min)


con <- DBI::dbConnect(RPostgres::Postgres(), dbname='gis', host='localhost',
                      port=5432, user='postgres', password='xxxxxx')

bbox <- alb %>% st_transform(3857) %>% st_bbox()

qry <- paste("SELECT NAME, ref, ST_Simplify(\"way\",50) AS way FROM new_york_osm_line WHERE way && ST_MakeEnvelope(",
             bbox[1],',',
             bbox[2],',',
             bbox[3],',',
             bbox[4],
             ", 3857) AND highway IS NOT NULL",sep="")

roads <- st_read(con, query=qry, geom='way') %>% st_intersection(alb %>% st_transform(3857))


ggplot(albb) + 
  geom_sf(aes(fill = min_time), color = NA) +
  geom_sf(data=roads, color='gray90', linewidth=0.1) +
  geom_sf(data=albfire, color='white') +
  geom_sf(data=alb, fill=NA, linewidth=0.6)+
  scale_fill_viridis_b(option = 'A', n.breaks=8, limits=c(0,10)) + 
  theme_void() + 
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: red; font-size: 45pt">Average Drive Time from Nearest Fire Station</span>'),
       fill = 'Time to Drive from Fire Station (minute)',
       tag = paste('<span style="color: gray20; font-size: 22pt">While fire trucks can exceed speed limits and go through red lights, this map shows how long it would take the average driver to get from a fire station to their block.</span><br /><span style="font-size:10pt">&nbsp;</span><br />',
                   'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br />Source: US Department of Homeland Security, MapBox, TidyCensus'),
       fill = "")  +
  theme(
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 1, hjust=1, face='bold',size=46, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555', width=0.7, valign=0, vjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(1.5,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.justification = 'left',
    legend.direction = 'horizontal',
    legend.position = c(0,0.5),
    legend.title = element_text(face='italic', size=12)
  ) +
  guides(fill=guide_colorsteps(ticks = F, title.position='bottom', title.hjust=0.5))
  

fn <- str_c('drive-time-fire')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

