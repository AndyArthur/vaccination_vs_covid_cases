library(tidyverse)
library(rvest)
library(ggtext)

salesdata <- read_html('https://www.tax.ny.gov/research/property/assess/sales/resmedian.htm')

nyco <- tigris::counties('ny',cb=T) %>% rmapshaper::ms_simplify()

salesdata %>% html_table() %>% .[[1]] %>%
  janitor::row_to_names(2) %>%
  janitor::clean_names() %>%
  mutate(across(2:ncol(.), ~parse_number(.)),
         county = str_replace(county, 'St ','St. ')) %>%
  left_join(nyco, ., by=c('NAME'='county')) %>%
  ggplot() + 
  geom_sf(aes(fill=median_3), linewidth=0.6, color='white') +
  scale_fill_viridis_b(labels=scales::label_dollar(scale = 1/1000, suffix = 'k'), breaks=seq(0,7)*1e5 ) +
  theme_void() +
  coord_sf(expand = F, crs = 3857) +
  labs(
    title = str_c(
      '<span style="font-size: 45pt; color: ', viridis::viridis(7)[1],
      '">Residential median sale price for 2021</span>'
    ),
    y = "",
    x = "",
    tag = paste(
      "Andy Arthur,",
      format(Sys.Date(), format = "%-m/%-d/%y"),
      "<br />Source: NYS ORPTS. ",
      "tax.ny.gov/research/property/assess/sales/resmedian.htm<br /><br />",
      "In order for a sale to be included in the above statistics it must be an arm's length residential sale coded non-condominium. Further, the sale price must be greater than ten dollars and the number of days between the sale date and the contract date must be less than three hundred and sixty five or indeterminate."
    ),
    fill = ""
  ) +
  theme(
    legend.key.height = unit(0.75, "cm"),
    legend.key.width = unit(3, "cm"),
    legend.position = c(0.27, 0.17),
    legend.spacing.y = unit(0.5, "cm"),
    legend.direction = "horizontal",
    text = element_text(family = "Lato", size = 14),
    plot.title = element_textbox(halign = 0.5, hjust = 0, face = "bold", size = 24, margin = unit(c(15, 0, 5, 0), "pt"), maxheight = 0, width = 0.4),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.subtitle = element_textbox(hjust = 0.5, halign = 0.5, margin = unit(c(5, 0, 5, 0), "pt")),
    plot.tag = element_textbox(size = 10, hjust = 0, color = "#555555", width=0.7, valign=0, vjust =0),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.tag.position = c(0.0, 0.01),
  )


fn <- str_c("liquor")
ggsave(paste("~/Desktop/", fn, ".jpg", sep = ""), width = 1920, height = 1200, units = "px", dpi = 120, device = grDevices::jpeg)
ggsave(paste("~/Desktop/", fn, ".svg", sep = ""), width = 1920, height = 1200, units = "px", dpi = 120, device = grDevices::svg)
system(paste("scour ~/Desktop/", fn, ".svg ~/Desktop/", fn, ".svgz", sep = ""))
unlink(str_c("~/Desktop/", fn, ".svg"))
