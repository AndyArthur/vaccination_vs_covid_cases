# load libraries
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)
options(tigris_use_cache=TRUE)
options(tigris_class="sf")

acs <- get_acs("block group", table = "B15003", cache_table = TRUE,
               geometry = TRUE, state = "36", county = "Schenectady",
               year = 2019, output = "tidy")

acs <- acs %>%
  mutate(
    id = str_extract(variable, "[0-9]{3}$") %>% as.integer
  ) %>%
  # variable 1 is the "total", which is just the sum of the others
  filter(id > 1) %>%
  mutate(education =case_when(
    id %>% between(2, 16) ~ "No HS diploma",
    id %>% between(17, 21) ~ "HS, no Bachelors",
    id == 22 ~ "Bachelors",
    id > 22 ~ "Post-Bachelors"
  )) %>% 
  group_by(GEOID, education) %>% 
  summarise(estimate = sum(estimate))

acs_split <- acs %>%
  filter(estimate > 50) %>%
  split(.$education)

generate_samples <- function(data) 
  suppressMessages(st_sample(data, size = round(data$estimate / 100)))

points <- map(acs_split, generate_samples)
points <- imap(points, 
               ~st_sf(data_frame(education = rep(.y, length(.x))),
                      geometry = .x))
points <- do.call(rbind, points)

points <- points %>% group_by(education) %>% summarise()
points <- points %>%
  mutate(education = factor(
    education,
    levels = c("No HS diploma", "HS, no Bachelors",
               "Bachelors", "Post-Bachelors")))
# view how many points are in each layer
points %>% mutate(n_points = map_int(geometry, nrow))


countysub <- county_subdivisions('ny', 'schenectady')

ggplot() + 
  geom_sf(data = points, 
          aes(colour = education,
              fill = education),
          size = 2, alpha = 0.5) + 
  scale_color_brewer(type = "div", palette = 4, name="") + 
  scale_fill_brewer(type = "div", palette = 4) +
  geom_sf(data = countysub, fill=NA) +
  ggsflabel::geom_sf_text_repel(data = countysub,
                                aes(label=`NAME`),
                                size=3, point.size = NA, fill='white', label.padding = unit(0.2, "lines"))+
  theme_void() +
  labs(title = paste('Educational Attainment in Schenectady County'),
       y = "",
       x = "",
       caption='2019 American Community Survey, Block Groups',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme(
    text= element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=26),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5, face = 'italic'),
    plot.tag=element_text(size=10,hjust=0),
    plot.caption=element_text(size=10),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'top'
  ) +
  guides(color = guide_legend(
    override.aes=list(shape = 16, size=5)), fill=F)


filename <- 'Education Attainment in Schenectady'
height <- 1400
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=height, units='px', dpi=150)
ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=height, units='px', dpi=150, device = grDevices::svg)
