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
  filter(between(month(DATE)*100+day(DATE), 620,719)) %>%
  group_by(`Year` = year(DATE)) %>%
  summarise(`Inches of Rain` = sum(PRCP, na.rm=T)) %>%
  ggplot(aes(Year,`Inches of Rain`, fill=`Inches of Rain`)) +
  geom_col() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_x_continuous(breaks = seq(1940, 2020, 5)) +
  scale_y_continuous(breaks=seq(0,15)) +
  coord_cartesian(expand = F) +
  theme_minimal() +
  labs(
    title = str_c('An <span style="color: darkblue; font-size: 56pt">Exceptionally Wet Month in Albany</span><br />(June 20 to July 19 Rainfall)'),
    y = "Inches of Rain",
    x = "",
    tag = paste("Andy Arthur,", format(Sys.Date(), format = "%-m/%-d/%y"), "<br /><em>Total Precipation from June 20 to July 19, Albany Airport"),
    fill = ""
  ) +
  theme(
    text = element_text(family = "Roboto Condensed", size = 18),
    plot.title = ggtext::element_textbox_simple(hjust = 0, size = 28, face = "bold", halign = 0.5),
    plot.background = element_rect(fill = "#FFFCFF", color = "#FFFCFF"),
    plot.tag = ggtext::element_textbox(size = 12, hjust = 1, color = "#555555", halign = 1, valign = 0),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.tag.position = c(1, 0),
    legend.position = "none"
  )

fn <- str_c("exceptionally-wet")
ggsave(paste("/tmp/", fn, ".jpg", sep = ""), width = 1920, height = 1200, units = "px", dpi = 120)
ggsave(paste("/tmp/", fn, ".svg", sep = ""), width = 1920, height = 1200, units = "px", dpi = 120, device = grDevices::svg)
system(paste("scour /tmp/", fn, ".svg /tmp/", fn, ".svgz", sep = ""))

source('upload-svg.R',local=T)

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
  filter(between(month(DATE)*100+day(DATE), 620,719)) %>%
  group_by(`Year` = year(DATE)) %>%
  summarise(`Inches of Rain` = sum(PRCP, na.rm=T)) %>%
  slice_max(`Inches of Rain`, n=20) %>%
  gt() %>%
  gtExtras::gt_theme_538() %>%
  tab_options(table.width = px(400)) %>%
  tab_footnote(html(
    'Andy Arthur, 7/21/23 <div style="float: right">NWS Observations, Albany Airport.')) -> output_table

output_table %>%
  tab_header(
    title = md(str_c("**Wettest Years**<br />Top 20 precipitation records from June 20 - July 19th at the Albany Airport."))
  ) -> output_with_header

output_with_header

output_with_header %>% gtsave('/tmp/output.png', expand=c(0,20,5,10)) 

wp.post.content <- output_table %>%
  as_raw_html(inline_css = F) 

wp.post.title <- 'Top ten precipitation records from June 20 - July 19th at the Albany Airpor'
source('upload-wordpress.R', local=T)


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
  filter(between(month(DATE)*100+day(DATE), 620,719)) %>%
  group_by(`Year` = year(DATE)) %>%
  summarise(`Inches of Rain` = sum(PRCP, na.rm=T)) %>%
  group_by() %>%
  summarise(rain = mean(`Inches of Rain`))

