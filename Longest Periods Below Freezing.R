library(tidyverse)
library(gt)

rm(list=ls())

wx <- read_csv('~/Downloads/3059447.csv')

wx %>% 
  mutate(BelowFreezing = TMAX < 32) %>%
  select(DATE, BelowFreezing) %>%
  group_by(consecutive_id(BelowFreezing)) %>%
  summarise(days = n(), BelowFreezing = first(BelowFreezing), start = first(DATE), end = last(DATE)) %>%
  filter(BelowFreezing) %>%
  select(Days = days, Start = start, End = end) %>%
  slice_max(Days, n=20) %>%
  gt() %>%
  tab_header('Longest Periods in Albany\'s History Below Freezing.','Number of days when the high temperature was 31 degrees or colder.') %>%
  tab_footnote(html('Andy Arthur, 2/6/23.<br /><em>Data Source:</em> National Weather Service Records, June 1938 to Present, Albany Airport')) %>%
  opt_stylize(style=3) %>%
  opt_row_striping() %>%
  fmt_date(2:3,date_style="yMMMEd") %>%
gtsave('/tmp/belowfreezing.png')

  
