library(tidyverse)
library(gt)

# looking at records from past 5 years
albnws <- read_csv('https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/USW00014735.csv.gz', col_names = F)

albnws %>%
  mutate(DATE = ymd(X2)
  ) %>%
  pivot_wider(id_cols = DATE, names_from = X3, values_from = X4) %>% 
  mutate(across(c(starts_with('T'),'ADPT'), ~ round((./10) *  9/5) + 32),
         across(c('PRCP', 'SNOW', 'SNWD'), ~round(./25.4)
         ),
         PRCP = PRCP/10,
         AWND = round(AWND * 2.237 / 10)
  ) %>%
  filter(month(DATE) == 7) %>%
  arrange(desc(year(DATE))) %>%
  group_by(Day = day(DATE)) %>%
  slice_max(PRCP, n=10, with_ties = F) %>%
  transmute(
    YrAmt = map(str_c(PRCP, '"<small><br />',  year(DATE), '</small>'), html), 
    Rank = row_number(),
    ) %>% 
  ungroup() %>%
  pivot_wider(names_from = Rank, values_from = YrAmt) %>%
  gt() %>%
  gtExtras::gt_theme_538() %>%
  data_color(
    rows = 10,
    columns = everything(),
    fn = \(x) "lightyellow"
  ) %>%
  data_color(
    rows = 13,
    columns = 2,
    fn = \(x) "lightblue"
  ) %>%
  tab_options(table.width = px(600)) %>%
  tab_footnote(html(
    'Andy Arthur, 7/8/23 <div style="float: right">NWS Observations, Albany Airport.')) -> output_table

output_table %>%
  tab_header(
    title = md("**July Precipitation Records**<br />Top ten precipitation observations from 1938-2022, Albany Airport.")
  ) -> output_with_header

output_with_header
  
output_with_header %>% gtsave('/tmp/output.png', expand=c(0,20,5,10)) 

wp.post.content <- output_table %>%
  tab_header(
    title = md("Top ten precipitation observations from 1938-2022, Albany Airport.")
) %>%
  as_raw_html(inline_css = F) 

wp.post.title <- 'July Precipitation Records'
source('upload-wordpress.R', local=T)

  
