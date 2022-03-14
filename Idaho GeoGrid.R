# required libraries
library(sf)
library(geogrid)
library(tidycensus)
library(tidyverse)

df <- get_decennial(geography = "county", state='maine',variables = "P1_001N", 
                     year = 2020, cache=TRUE, geometry = T) %>%
  st_set_crs('4326')

tmp <- calculate_grid(shape = df, grid_type = "hexagon", seed = 2)
df_hex <- assign_polygons(df, tmp) 
df_hex = st_as_sf(df_hex)

df_hex <- df_hex %>% separate(NAME, c('NAME'), ',')

ggplot(df_hex) + geom_sf(aes(fill=value), color='white', size=3) + 
  scale_fill_viridis_c(option = 'C') +
  scale_y_continuous(expand=c(0,0), position='left',
      sec.axis = sec_axis( trans=~., name="") )+
  ggsflabel::geom_sf_label_repel(
    aes(label=paste(scales::comma(value, accuracy=1), NAME, sep="\n")),
    size=3, point.size = NA, fill='white', label.padding = unit(0.2, "lines")) +
  theme_void() +
  labs(title = '',
       y = "Maine 2020 Population",
       x = "",
       caption='2020 US Census PL 94-171 Data',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=26),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5, face = 'italic'),
    plot.tag=element_text(size=10,hjust=0),
    axis.title.y.left = element_text(angle=90, face='italic'),
    axis.title.y.right = element_text(angle=-90, face='bold',size=36),
    plot.caption=element_text(size=10),
    legend.margin = margin(10,0,0,0),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'None'
  )

filename <- 'MO_Population'
height <- 1800
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=150, device = grDevices::svg)
