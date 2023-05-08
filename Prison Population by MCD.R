library(tidyverse)
library(tidycensus)
library(tigris)

pa <- readxl::read_excel('/tmp/PL_ADJUSTED_DOJ_MCD.xlsx',skip=1) %>%
  mutate(GEOID = str_c(STATE, COUNTY, MCD))

op <-  get_decennial(geography = "county subdivision", variables = "P1_001N", 
                          year = 2020, cache=TRUE, geometry = F, state='ny')


op %>% left_join(pa) %>%
  transmute(
    GEOID,
    NAME,
    Prison.Pop = TOTAL_ADJ - value,
    Prison.Pop = ifelse(value < 20000, 0, Prison.Pop),
    Adj.Pop = TOTAL_ADJ,
    Prison.Per.10k = Prison.Pop/TOTAL_ADJ*10000
  ) -> prisonpop

nycos <- county_subdivisions('ny', cb=T)
nyco <- counties('ny', cb=T)


nycos %>% 
  left_join(prisonpop, join_by(GEOID)) %>%
  ggplot() +
  geom_sf(aes(fill=Prison.Per.10k)) +
  geom_sf(data=nyco, fill=NA, linewidth=0.5) +
  scale_fill_viridis_b(n.breaks=8, name='', option='B') +
  coord_sf(expand=F) +
  theme_void() + 
  labs(tag = str_c('<span style=""><b style="font-size: 43pt; color: darkred">Residents in State Prison</b><br /> per 10,000 residents (2022)</span><br />',
                   '<br />Andy Arthur, 4/26/23<br /><br />
                   <span style="font-size: 14pt">Methodology based on Prison Policy Initative, prisonpolicy.org, essentially (\'20 LATFOR Adj Pop - \'20 Census Pop) / 10,000
                   '),
  )  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.tag=ggtext::element_textbox(hjust=0, halign=0, face='bold', height=0, size=24, width=0.65),
    plot.tag.position = c(0,1),
    plot.background = element_rect(fill = "mintcream", color="mintcream"),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    legend.position = c(0, 0.1),
    legend.direction = 'horizontal',
    legend.justification = c(0,0),
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(3.5,'cm')
  )

fn <- str_c('ma-landcover')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

