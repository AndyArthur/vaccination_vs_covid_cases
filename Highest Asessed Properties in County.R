library('RPostgreSQL')
library(dplyr)
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

con <- DBI::dbConnect(RPostgres::Postgres(), dbname='gis', host='localhost', 
                      port=5432, user='postgres', password='farmlove')

# get columns list
sql <- 'select *
from  nytax_ctr_point 
where false;'
dbGetQuery(con, sql)

sql <- 'DROP TABLE propTot'
dbExecute(con, sql)

sql <- '
      SELECT
        primary_owner,
        county_name,
        avg(ST_X(geom)) as x,
        avg(ST_Y(geom)) as y,
        sum(acres) as av
      INTO TEMPORARY propTot
      FROM
        nytax_ctr_point
      WHERE
        "acres" < 2000
      GROUP BY  
        primary_owner, county_name
'
dbExecute(con, sql)

sql <- '
  SELECT 
    t1.*
  FROM
    propTot t1
  RIGHT JOIN (
    SELECT county_name, max(av) as av
    FROM propTot
    GROUP BY county_name
  ) t2
  ON t1.county_name = t2.county_name AND
  t1.av = t2.av
  WHERE t1.av > 0
  '
df <- dbGetQuery(con, sql)

df %>% View()

df <- st_as_sf(df, coords=c('x','y'), crs = 'EPSG: 26918')


county <- counties(state='ny', cb=T)

ggplot(df) +
  geom_sf(data=county,size=0.3, fill='gray90') +
  geom_sf() +
  ggsflabel::geom_sf_label_repel(aes(label=str_to_title(primary_owner)), size=2.1,
                               box.padding = 0.5, point.size = 4, min.segment.length = unit(0.5, "lines")) +
  theme_void() +
  coord_sf(expand = F, crs='EPSG: 3857') +
  labs(title = 'Property Owner with the Highest Total Acerage, by County',
       subtitle = paste('This includes all privately owned properties of Owner Type 8 on Section 1 of the tax rolls, sum of total',
        'acerage outside of New York City.', sep="\n"),
       x='',
       y='',
       caption='NYS Parcel Program',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=26),
    plot.background = element_rect(fill = "Grey76", color=NA),
    plot.margin=unit(c(5,5,5,5), 'pt'),
    plot.subtitle=element_text(hjust=0.5, face = 'italic'),
    plot.tag=element_text(size=10,hjust=0),
    axis.title.y.left = element_text(angle=90, face='italic'),
    axis.title.y.right = element_text(angle=-90, face='bold',size=36),
    plot.caption=element_text(size=10),
    legend.margin = margin(10,0,0,0),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'None'
  )

filename <- 'Highest_Acerage'
height <- 1300
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=130)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=130, device = grDevices::svg)

system(paste('scour /tmp/',filename,'.svg', ' /tmp/',filename,'.svgz',sep=''))  
