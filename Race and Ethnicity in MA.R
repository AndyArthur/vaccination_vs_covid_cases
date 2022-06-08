library(tidycensus)
library(tidyverse) 
library(wacolors)
library(ggtext)

race <- c(
  White = "P1_003N",
  Black = "P1_004N",
  Native = "P1_005N",
  Asian = "P1_006N",
  Hawaiian = "P1_007N",
  Hispanic = 'P2_002N'
)

pop2020 <- get_decennial(geography = "tract", state='ca', variables = race,
                         summary = "P1_001N",  year = 2020, geometry = T) %>%
  rmapshaper::ms_simplify()

pop2020 %>% mutate(percent = (value/summary_value)*100) -> pop2020

ggplot(pop2020) + geom_sf(aes(fill=percent)) + facet_wrap(~variable)

ggplot(pop2020) + 
  geom_sf(aes(fill=percent), size=0) + 
  scale_fill_wa_c(palette = 'ferries', trans='log10', labels=scales::percent_format(accuracy = 0.1, scale = 1)) + 
  facet_wrap(~variable) +
  labs(title = str_c('<span style="color: brown">Race and Ethnicity</span> of <span style="color: orange">California</span>'),
       y = "",
       x = "",
       caption='2020 US Census',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_void() +
  theme(
    text= element_text(family='Noto Sans',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=30, margin=unit(c(5,0,3,0),'pt'), lineheight = 0.5),
    plot.background = element_rect(fill = "white", color="white"),
    strip.text = element_text(family='bold'), 
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, face='italic', margin=unit(c(0,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(0.3,'cm'),
    legend.key.width = unit(5,'cm'),
    legend.position = 'top', 
  ) 

fn <- 'ca-race'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

