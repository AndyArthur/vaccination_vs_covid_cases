library(tidycensus)
library(tidyverse)
library(tigris)

census_api_key("2e5efe5ba21d5ac8e0dd34ccd85db4a364cef118", install = TRUE)
options(tigris_use_cache = TRUE)

us_median_age <- get_acs(
    geography = "county",
    variables = "B01002_001",
    survey = "acs5",
    year = 2021,
    keep_geo_vars = TRUE,
    geometry = TRUE,
    resolution = "20m"
) %>%
    shift_geometry() %>%
    rmapshaper::ms_simplify()

usst <- states(cb = T, resolution = "20m") %>%
    shift_geometry() %>%
    rmapshaper::ms_simplify(0.5)

ggplot(data = us_median_age, aes(fill = estimate)) +
    geom_sf(linewidth = 0.1, color = "white") +
    geom_sf(data = usst, fill = NA, linewidth = 1, color = "white") +
    scale_fill_fermenter(
        palette = "RdGy",
        direction = 1, breaks = seq(30, 50, 5)
    ) +
    coord_sf(expand = F) +
    theme_void() +
    labs(
        title = str_c('<span style="font-weight: bold; font-size: 50px">Median Age by County</span>'),
        y = "",
        x = "",
        tag = str_c(
            "Andy Arthur, ", format(Sys.Date(), format = "%m/%d/%y"),
            "<br />Data Source: 2021 American Community Survey, 5-yr Averages. Table B01002."
        ),
        fill = ""
    ) +
    theme(
        text = element_text(family = "Roboto Condensed", size = 18),
        plot.title = ggtext::element_textbox_simple(hjust = 0, size = 20, width = 0.5),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.tag = ggtext::element_textbox(size = 12, hjust = 1, color = "#555555", maxheight = 0, height = 0, halign = 1, valign = 0),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.tag.position = c(1, 0),
        strip.text = element_text(face = "bold"),
        legend.key.height = unit(0.7, "cm"),
        legend.key.width = unit(3, "cm"),
        legend.direction = "horizontal",
        legend.position = c(1, 1.03),
        legend.justification = "right"
    )

fn <- str_c("countyage")
ggsave(paste("/tmp/", fn, ".jpg", sep = ""), width = 1920, height = 1200, units = "px", dpi = 120)
ggsave(paste("/tmp/", fn, ".svg", sep = ""), width = 1920, height = 1200, units = "px", dpi = 120, device = grDevices::svg)
system(paste("scour /tmp/", fn, ".svg /tmp/", fn, ".svgz", sep = ""))
