library(sf)
library(tidycensus)
library(tidyverse)
library(ggtext)
rm(list=ls())

# load latfor data, VTD level data is okay for larger districts
# although latfor also provides block level data which is better as
# those centroids should always be inside the district you are comparing
vt20 <- read_csv('~/2020vote_vtd.csv')
vt20$GEOID <- as.character(vt20$GEOID)

asian <- get_decennial(geography = "voting district", state='ny', variables = c("P2_001N","P2_002N"), 
              year = 2020, cache=TRUE, geometry = T, sumfile = 'pl')

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

vt20 <- vt20 %>% dfADP(20)

asian %>% 
  pivot_wider(names_from = 'variable', values_from = 'value') %>%
  mutate(asian_per = P2_002N/P2_001N) %>%
  inner_join(vt20, by='GEOID') %>%
  ggplot(aes(x=asian_per, y=dem/ballot, fill=dem/ballot)) + 
  geom_point(size=1.3, shape=21, stroke=0.05, color='black') +
  geom_smooth(color='black', se = F) +
  ggredist::scale_fill_party_c(name='') +
   scale_y_continuous(labels=scales::label_percent(), limits=c(0,1), breaks=seq(0,1,.1)) +
  scale_x_continuous(labels=scales::label_percent(), limits=c(0,1), breaks=seq(0,1,.1)) +
  coord_cartesian(expand=F) + 
  labs(
    title = str_c('<span style="color: gray20; font-size: 30pt;">How Hispanics Voted in the NY 2020 Elections</span><br />
                  <span style="font-size: 18pt; color: gray20">Average Democratic Performance, all competitive races by Census VTD'),
    y = "2020 Average Democratic Performance",
    x = "Percent Hispanic",
    caption = "",
    tag = paste("Source: LATFOR, PL 94-171 Data<br />Andy Arthur,", format(Sys.Date(), format = "%m/%d/%y"), ""),
    fill = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Roboto Condensed", size = 14),
    plot.title = element_textbox(hjust = 0, face = "bold", size = 28, margin = margin(0, 0, 10, 0)),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.tag = ggtext::element_textbox(size = 10, hjust = 1, color = "#555555", maxheight = 0, halign = 1),
    plot.caption = element_text(size = 10, color = "#555555"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.tag.position = c(1, 0.03),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.direction = "horizontal",
    legend.position = c(1, 1.05),
    legend.justification = "right",
  )

fn <- "hispanics-vote"
height <- 1250
ggsave(paste("/tmp/", fn, ".jpg", sep = ""), width = 1920, height = height, units = "px", dpi = 130)
ggsave(paste("/tmp/", fn, ".svg", sep = ""), width = 1920, height = height, units = "px", dpi = 130, device = grDevices::svg)
system(str_c("scour /tmp/", fn, ".svg /tmp/", fn, ".svgz"))

