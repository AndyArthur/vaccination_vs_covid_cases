library(tidyverse)
library(janitor)
library(biscale)
library(sf)
library(cowplot)
library(tidycensus)
library(ggtext)

# data

data <- get_acs(geography = "county",
                variables = c(total = "B02001_001",
                              white_alone = "B02001_002",
                              median_household_income = "B19013_001"),
                output = "wide",
                state = c("NY"),
                year = 2021,
                geometry = T) %>% 
  mutate(white_percent = white_aloneE / totalE)

# map

bi_data <- bi_class(data, x = white_percent, y = median_household_incomeE, style = "quantile", dim = 3)

bi_map <- ggplot() +
  geom_sf(data = bi_data, mapping = aes(fill = bi_class), color = "white", linewidth=0.8, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueGold", dim = 3) +
  bi_theme() +
  labs(title = "<span style='color:#0072B2;'>Race</span> and <span style='color:#D4A017;'>income</span> in New York",
    tag = paste("Source: US Census Bureau, Median Household Income, Race, 2021 ACS 5 yr <br />Andy Arthur,", format(Sys.Date(), format = "%-m/%-d/%y"), ""),
    fill = ""
  ) +
coord_sf(expand=F) +
theme_void() +
  theme(
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold', size=65, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width=0.7, color='gray30', maxwidth = 0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  ) 

legend <- bi_legend(pal = "BlueGold",
                    dim = 3,
                    xlab = "Higher % white",
                    ylab = "Higher income ",
                    pad_width = 1.2,
                    size = 12)

plot <- ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, 0.05, 0.25, 0.25) +
  theme(
    plot.background = element_rect(fill='white', color='white')
  )

fn <- str_c('income-race')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120,  device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))

