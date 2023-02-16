library(tidyverse)
library(lubridate)
library(gt)

rm(list=ls())

wx <- read_csv('~/Downloads/3059447.csv')

wx %>% 
  mutate(
    NumericDate = month(DATE)*100 + day(DATE),
    AboveFreezing = (TMIN > 31 & (NumericDate < 322 | NumericDate > 1220))) %>%
  select(DATE, AboveFreezing) %>%
  group_by(consecutive_id(AboveFreezing)) %>%
  summarise(days = n(), AboveFreezing = first(AboveFreezing), start = first(DATE), end = last(DATE)) %>%
  filter(AboveFreezing) %>%
  select(Days = days, Start = start, End = end) %>%
  slice_max(Days, n=20) %>%
  arrange(-Days, rev(Start)) %>%
  group_by(Days = str_c(Days, ' days ...')) %>%
  gt() %>%
  tab_header('Longest Winter Periods Above Freezing','Number of days when the low temperature was 32 degrees or above from December 21 - March 21.') %>%
  tab_footnote(html('Andy Arthur, 2/6/23.<br /><em>Data Source:</em> National Weather Service Records, June 1938 to Present, Albany Airport.')) %>%
  opt_stylize(style=3, color = 'red') %>%
  opt_row_striping() %>%
  fmt_date(2:3,date_style="yMMMEd") %>%
 gtsave('/tmp/AboveFreezing.html')

  
