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
        muni_name,
        avg(ST_X(geom)) as x,
        avg(ST_Y(geom)) as y,
        sum(acres) as av
      INTO TEMPORARY propTot
      FROM
        nytax_ctr_point
      WHERE
        "roll_section"=\'1\' AND
        "owner_type"=\'8\' AND
        "acres" < 1000
      GROUP BY  
        primary_owner, county_name, muni_name
'
dbExecute(con, sql)

sql <- '
  SELECT 
    t1.*
  FROM
    propTot t1
  INNER JOIN (
    SELECT county_name, muni_name, max(av) as av
    FROM propTot
    GROUP BY county_name, muni_name
  ) t2
  ON 
  t1.muni_name = t2.muni_name AND
  t1.av = t2.av
  WHERE t1.av > 0
    '
df <- dbGetQuery(con, sql)

df %>% View()

df <- st_as_sf(df, coords=c('x','y'), crs = 'EPSG: 26918')

df %>% write_sf('/tmp/muni.gpkg')
