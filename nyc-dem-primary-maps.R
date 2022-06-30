library(tidyverse)
library(rvest)
library(janitor)
library(sf)
library(ggtext)

rm(list=ls())

urls <- c(
  'https://web.enrboenyc.us/CD24989AD244.html',
  'https://web.enrboenyc.us/CD25173AD284.html',
  'https://web.enrboenyc.us/CD25223AD294.html',
  'https://web.enrboenyc.us/CD25598AD304.html',
  'https://web.enrboenyc.us/CD25598AD304.html',
  'https://web.enrboenyc.us/CD24971AD354.html',
  'https://web.enrboenyc.us/CD24867AD374.html',
  'https://web.enrboenyc.us/CD24948AD404.html',
  'https://web.enrboenyc.us/CD24866AD433.html',
  'https://web.enrboenyc.us/CD24967AD463.html',
  'https://web.enrboenyc.us/CD24892AD503.html',
  'https://web.enrboenyc.us/CD24966AD513.html',
  'https://web.enrboenyc.us/CD24853AD543.html',
  'https://web.enrboenyc.us/CD25073AD553.html',
  'https://web.enrboenyc.us/CD24876AD573.html',
  'https://web.enrboenyc.us/CD24886AD583.html',
  'https://web.enrboenyc.us/CD25006AD603.html',
  'https://web.enrboenyc.us/CD25549AD610.html',
  'https://web.enrboenyc.us/CD24922AD650.html',
  'https://web.enrboenyc.us/CD24871AD660.html',
  'https://web.enrboenyc.us/CD25049AD681.html',
  'https://web.enrboenyc.us/CD25213AD701.html',
  'https://web.enrboenyc.us/CD24957AD710.html',
  'https://web.enrboenyc.us/CD25332AD721.html',
  'https://web.enrboenyc.us/CD24870AD730.html',
  'https://web.enrboenyc.us/CD24831AD750.html',
  'https://web.enrboenyc.us/CD24858AD761.html',
  'https://web.enrboenyc.us/CD24863AD780.html',
  'https://web.enrboenyc.us/CD24863AD780.html',
  'https://web.enrboenyc.us/CD24828AD812.html',
  'https://web.enrboenyc.us/CD24888AD822.html',
  'https://web.enrboenyc.us/CD24936AD842.html',
  'https://web.enrboenyc.us/CD25357AD862.html'
)

for (url in urls) {
    
  ad <- substr(url, str_length(url)-7, str_length(url)-6)
  
  tb <- read_html(url) %>% html_table()
  
  race <- row_to_names(tb[[3]],1) %>%
    remove_empty(which='cols')
  
  colnames(race)[c(1,2)] <- c('ED', 'Reported')
  race <- race[c(-1,-nrow(race)),-2]
  
  race <- race %>% 
    mutate(across(1:ncol(.), ~parse_number(.)))
    
  # change to your pth of nyc eds -- https://www1.nyc.gov/site/planning/data-maps/open-data.page
  nyced <- read_sf('Desktop/nyc-ed.gpkg') 
    
  shp <- race %>% 
    rowwise() %>%
    mutate(total = sum(across(2:ncol(.))), 
           across(2:ncol(.), ~(./total)*100)
           ) %>%
    select(-total) %>%
    mutate(maxval = max(across(2:ncol(.))), across(2:ncol(.), ~ifelse(maxval == ., ., NA) )) %>%
    select(-maxval) %>%
    mutate(ED = parse_number(str_c(ad, str_pad(ED, 3, pad = "0")))) %>%
    mutate(across(everything(), ~replace_na(.,0))) %>%
    left_join(nyced, by = c('ED' = 'ElectDist')) %>%
    pivot_longer(2:`WRITE-IN`) %>%
    drop_na() %>%
    st_set_geometry('geom')
  
  adbound <- shp %>% distinct(geom) %>% st_union()
  
  bbox <- adbound%>% st_transform('EPSG: 3857')%>% st_bbox()
  
  # update paths to the location of your open street map data
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname='gis', host='localhost', 
                        port=5432, user='postgres', password='******')
  
  qry <- paste("SELECT way FROM new_york_osm_line WHERE way && ST_MakeEnvelope(",
               bbox[1],',',
               bbox[2],',',
               bbox[3],',',
               bbox[4],
               ", 3857) AND highway IS NOT NULL AND highway IS NOT NULL",sep="")
  
  roads <- st_read(con, query=qry, geom='way') %>% st_intersection(adbound)
  
  qry <- paste("SELECT way FROM new_york_osm_polygon WHERE way && ST_MakeEnvelope(",
               bbox[1],',',
               bbox[2],',',
               bbox[3],',',
               bbox[4],
               ", 3857) AND building='yes'",sep="")
  
  blding <- st_read(con, query=qry, geom='way')%>% st_intersection(adbound)
  
  qry <- paste("SELECT name, ST_Simplify(way,500) as way FROM new_york_osm_point WHERE way && ST_MakeEnvelope(",
               bbox[1],',',
               bbox[2],',',
               bbox[3],',',
               bbox[4],
               ", 3857) AND place IS NOT NULL",sep="")
  
  neig <- st_read(con, query=qry, geom='way') %>% st_intersection(adbound)
  
  bbox_new <- bbox
  xrange <- bbox_new$xmax - bbox_new$xmin 
  yrange <- bbox_new$ymax - bbox_new$ymin
  
  bbox_new[1] <- bbox_new[1] - (0.5 * xrange) # xmin - left
  bbox_new[3] <- bbox_new[3] + (0.5 * xrange) # xmax - right
  
  bbox_new <- st_as_sfc(bbox_new)
  
  ggplot(shp) +
    geom_sf(aes(fill=name, alpha=value)) +
    geom_sf(data=roads, size=0.05) +
    geom_sf(data=blding, size=0, alpha=0.4, fill='black') +
    geom_sf(data=bbox_new, color=NA, fill=NA) +
    ggsflabel::geom_sf_label_repel(data=neig, aes(label=name), size=2.5, family='Noto Sans') +
    scale_fill_brewer(palette = 'Spectral', name='  ', labels=stringr::str_to_title, na.value='grey') +
    scale_alpha(range = c(0,1), name='', labels=scales::percent_format(scale=1)) +
    theme_void() +
    guides(alpha = guide_legend(override.aes = list(fill = 'black')),
           fill = guide_legend(override.aes = list(size = 0)),
    ) +
    coord_sf(expand=F) +
    labs(title = str_c('<span style="color: darkblue">AD ', ad, '</span> <span style="color: blue">Democratic Primary</span><br /><span style="font-size: 20pt">Election Night Results</span>'),
         # subtitle = 'Thomas Wolf (D) vs Scott Wagner (R) with each county\'s results scaled to the total vote of county.',
         y = "",
         x = "",
         caption=url,
         tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
         fill = "") +
    theme(
      text= element_text(family='Noto Sans',size=14),
      plot.title=element_textbox_simple(halign = 0.5, hjust=0.5, face='bold',
                                        size=26, margin=unit(c(0,0,.5,0), 'lines')),
      plot.subtitle=element_textbox_simple(halign = 0.5, hjust=0.5, face='italic',
                                           margin=unit(c(0,0,.5,0), 'lines')),
      
      plot.background = element_rect(fill = "white", color="white"),
      plot.tag=element_text(size=10,hjust=0, color='#555555'),
      plot.caption=element_text(size=10, color='#555555'),
      plot.margin = unit(c(1,1,1,1), 'lines'),
      plot.tag.position = c(0.0,0.01),
      legend.title.align = 0.5,
      legend.direction = 'horizontal',
      legend.position = 'top'
    ) 
  
  fn <- str_c('ad-primary/ad',ad,'-primary')
  ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=140)
  system(str_c('mogrify -trim /tmp/',fn,'.jpg'))
}
