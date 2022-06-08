library(tidyverse)
library(tigris)
library(ggtext)
library(sf)

rm(list=ls())

# load latfor data, VTD level data is okay for larger districts
# although latfor also provides block level data which is better as
# those centroids should always be inside the district you are comparing
vt20 <- read_csv('2020vote_vtd.csv')
vt20$GEOID <- as.character(vt20$GEOID)

# edADP - This code has to be as efficient as possible, as
# it can not be vectorized, due to the need to check to see
# if a race is competitive before including. We also don't want
# to search for columns here, due to the CPU time cost over thousands
# of rows, each time searched.
edADP <- function(ed, demcols) {
  
  # making sure everything is numeric helps with speed
  ed <- ed %>% as.numeric
  
  # store a total of dems, rep, ballot votes 
  dem <- 0; rep <- 0; ballot <- 0;
  
  # use democratic columns as an index for all other columns
  for (pos in demcols) {
    
    # if either Democratic or Republican line is 0, then that 
    # multiplies out to zero as x * 0 = 0. Then negate and skip to next race
    if (!(ed[pos] *  ed[pos+1])) next;
    
    # to minimize costs, we just use row positions based on positition relative to
    # democratic rows
    dem <- dem + ed[pos]
    rep <- rep + ed[pos+1]   
    ballot <- ballot + ed[pos-1] - ed[pos+6]
  }
  
  # return the vote totals for all competitive races to bind to the main table
  return (c('dem'=dem, 'rep'=rep, 'ballot'=ballot))
}

dfADP <- function(df, yr=18) {
  # find position of dem cols before starting to minimize CPU costs when
  # doing the row-by-row math with apply
  demcols <- c()
  
  i <- 1
  for (dfcols in colnames(df)) {
    if (substr(dfcols, 1, 5) == str_c('DEM',yr)) demcols <- append(demcols, i)
    i <- i+1
  }
  
  # Calculate ADP, row by row then bind to the dataframe
  return(cbind(df,df %>% apply(1, edADP, demcols) %>% t))
}

# calculate ADP from VT20 data
adp <- vt20 %>% dfADP(18) 

# join the VTD data to voting districts
vtd <- voting_districts('ny', cb=T) %>%
  inner_join(adp, by=c('GEOID20'='GEOID')) %>%
  st_transform('epsg:3857')

# load the shapefile want to join the VTDs against
a22 <- read_sf('/home/andy/Documents/GIS.Data/2022-districts/2022-sd-may20-court-final.gpkg') %>% st_transform('epsg:3857')

nad <- vtd %>% st_centroid() %>% 
  st_join(a22) %>% st_drop_geometry() %>% group_by(id) %>% 
  summarise(ADP = (sum(dem)/sum(ballot))*100, 
            dem=sum(dem), 
            rep=sum(rep),
            ballot=sum(ballot)
  )

a22 %>% inner_join(nad, by=c('id'='id')) %>% write_sf('/tmp/adp-sd.gpkg')

vtd %>% write_sf('/tmp/adp-vtd.gpkg')









name <- 'Congress Cervas SM'

# for reasons of speed, convert VTDs to centroids, then join against 
# the new districts
join <- vtd %>% st_centroid() %>% 
  st_join(a22) 

join$NAME <- as.numeric(join$NAME)

# calculate the ADP by dividing democratic votes in all competitive races against
# democrats and republican. Add column for coloring
join %>% st_drop_geometry() %>% group_by(NAME) %>% 
  summarise(ADP = (sum(dem)/sum(dem+rep))*100,
            color=ifelse(ADP > 60, 'Democratic (>60%)', 
                         ifelse(ADP > 50, 'Lean Democratic (>50%)', 
                                ifelse(ADP > 40, 'Lean Republican (>40%)', 'Republican (<40%)')))
            ) %>% drop_na() %>% 
  ggplot(aes(NAME, ADP, fill=color)) + geom_col(alpha=0.8) +
  geom_hline(yintercept = 50) +
  scale_fill_manual(values=c('blue','LightSkyBlue','Pink','Red')) +
  scale_x_continuous(breaks=seq(1,64,1)) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) +
  theme_minimal() + 
  coord_cartesian(expand=F) +
  labs(
    title = str_c('<span style="color: red;">2018 Average</span> <span style="color: blue;">Democratic Performance</span> - ',name,' \'22'),
    y = "2018 ADP (All races with Dem. and Rep. candidates)",
    x = "",
    caption='Data Source: LATFOR VTDs',
    tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme(
    text=element_text(family='Open Sans',size=14),
    plot.title=element_textbox( hjust=0.5, face='bold',size=34),
    axis.text = element_text(size=12),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(4,'cm'),
    legend.key.height = unit(0.2,'cm'),
    legend.position = 'top'
  )


fn <- str_c('22-',name,'-graph')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)


# calculate the ADP by dividing democratic votes in all competitive races against
# democrats and republican. Add column for coloring
join$NAME <- as.character(join$NAME)

join %>% st_drop_geometry() %>% group_by(NAME) %>% 
  summarise(ADP = (sum(dem)/sum(dem+rep))*100, 
            color=ifelse(ADP > 60, 'Democratic (>60%)', 
            ifelse(ADP > 50, 'Lean Democratic (>50%)', 
            ifelse(ADP > 40, 'Lean Republican (>40%)', 'Republican (<40%)')))) %>% 
  drop_na() %>% 
  inner_join(a22, by=('NAME')) %>%
  ggplot(aes(geometry=geometry, fill=color.x)) + geom_sf(size=0.5) +
  scale_fill_manual(values=c('blue','LightSkyBlue','Pink','Red')) +
  theme_void() + 
  coord_sf(expand=F) +
  labs(
    title = str_c('<span style="color: red;">2018 Average</span> <span style="color: blue;">Democratic Performance</span> - ',name,' \'22'),
    caption='Data Source: LATFOR VTDs',
    tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme(
    text=element_text(family='Open Sans',size=14),
    plot.title=element_textbox( hjust=0.5, face='bold',size=34, margin=unit(c(0,0,1,0),'pt')),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.width = unit(4,'cm'),
    legend.key.height = unit(.6,'cm'),
    legend.position = 'top'
  )

fn <- str_c('22-',name,'-map')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
