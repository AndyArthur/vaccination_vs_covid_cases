# load libraries
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)

rm(list=ls())

vars <- load_variables(2021, 'acs5', cache=T)

acs <- get_acs("state", survey='acs5', variable = "B19013_001", cache_table = T,
               geometry = T,
               year = 2020, output = "wide")


acs <- acs %>% mutate(group = cut(acs$B19013_001E, classInt::classIntervals(acs$B19013_001E, 4, cutlabels=F)$brks, 
                           labels = c('Poorest', 'Below Average', 'Above Average', 'Wealthiest'))) %>%
  drop_na() %>%
  shift_geometry() %>% rmapshaper::ms_simplify()

ggplot(acs) + 
  geom_sf(aes(fill=group), linewidth=1, color='ivory') +
  wacolors::scale_fill_wa_d(palette = 'sound_sunset', reverse = F, na.value='gray',  guide = guide_legend(reverse = TRUE)) +
  coord_sf(expand=F) +
  theme_void() + 
  labs(title = str_c('<span style="font-size: 45pt">Median Household Income</span>',
                     '<br /><span style="font-size: 26pt">A look at states broken down by quantile for their Median Household Income.</a>'),
       y = "",
       x = "",
       tag=paste('2021 ACS 5-yr American Community Survey, Table B19013<br />',
                 'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple(hjust=0, face='bold'),
    plot.background = element_rect(fill = "ivory", color="ivory"),
    plot.tag=ggtext::element_textbox(size=12, hjust=1, color='#555555', maxheight=0, halign = 1, width=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.75,0.03),
    legend.position = c(1,0.5),
    legend.justification = 'right',
    legend.key.height = unit(0.3,'cm'),
    legend.key.width = unit(0.6,'cm'),
    legend.text = element_text(margin = margin(t = 30, unit = "pt")),
  ) 

fn <- str_c('median-hh-quantile')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))



library(gt)
library(gtExtras)

acs %>%
  st_drop_geometry() %>% 
  arrange(-B19013_001E) %>%
  mutate(Ranking = row_number()) %>%
  arrange(-B19013_001E) %>%
  transmute(State = NAME, `Median Household Income` = B19013_001E, `Compared to Others` = group, Ranking) %>%
  gt() %>%
  fmt_currency(2, decimals = 0) %>%
  #gt_color_rows(2, domain = c(46,91)*1e3, palette = rev(RColorBrewer::brewer.pal(8, 'RdYlBu')))%>%
 # cols_width(2:4 ~ px(150)) %>%
  opt_stylize(style=1, color='gray') %>%
  tab_header('Median Household Income (2021)','A look at the states and their relative wealth (quantiles).') %>%
  tab_footnote(html('Andy Arthur, 1/31/23.<br /><em>Data Source:</em> 2021 American Community Survey 5 Yr, Median Household Income')) %>%
  gtsave('/tmp/wealthy.html')

