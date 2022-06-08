library(tigris)
library(tidycensus)
library(sf)
library(ggtext)

get_acs(
  geography = "tract",
  state='mi',
  variables = "B19013_001",
  year = 2020,
  survey = "acs5",
  geometry = T
) -> income

counties('mi',cb=T) -> mico

mico %>% filter(NAME != 'Keweenaw') %>% st_bbox() -> mibbox

ggplot(income) + 
  geom_sf(aes(fill=estimate),size=0) +
  scale_fill_distiller(palette = 'PuBuGn', trans='log10',  label=scales::dollar_format(accuracy = 1)) +
  geom_sf(data=mico, fill=NA,size=0.3) +
  labs(title = str_c('<span style="color: darkgreen">Median Household Income</span> in <span style="color: darkblue">Michigan</span>'),
       y = "",
       x = "",
       caption='2020 American Community Survey, 5-Yr',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=30, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'top',
  ) +
  coord_sf(expand=F, xlim = mibbox[c(1,3)],  ylim = mibbox[c(2,4)]) 

fn <- str_c('mi-income')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

