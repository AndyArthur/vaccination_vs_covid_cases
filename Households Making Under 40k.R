library(tidycensus)
library(tidyverse)
library(ggtext)

rm(list=ls())

#cvar <- load_variables(2021, 'acs5')
rm(cvar)

acs <- get_acs(
  geography = 'county',
  variables = str_c('B19001_00',seq(2,8)),
  summary_var = 'B19001_001',
  year=2021,
  output='wide', 
  state='ny',
  geometry = T
)


acs <- acs %>% 
  rmapshaper::ms_simplify() %>% 
  st_transform(3857)


acs %>% mutate(`under40k` = rowSums(across(starts_with('B19001_00')))) %>%
                 group_by() %>%
                 summarise(under40k = sum(under40k), summary_est = sum(summary_est), percent = sum(under40k)/sum(summary_est))

acs %>%
  mutate('under40k' = rowSums(across(starts_with('B19001_00'))/summary_est)) %>%
  ggplot() + geom_sf(aes(fill=under40k), color='white', linewidth=0.8) +
  ggsflabel::geom_sf_text_repel(aes(label=scales::percent(under40k, accuracy = 1)),
                                size=3, point.size = NA, box.padding = unit(0.2, "lines"),
                                bg.r=0.2, bg.color='#ffffffcc', fontface='bold', lineheight=0.8,
                                min.segment.length =0, family='Lato') +
  scale_fill_viridis_b(labels=scales::label_percent(), name='', n.breaks=7) +
  coord_sf(expand=F, crs=3857) +
  theme_void() + 
  labs(title = str_c('<span style="color: navy; font-size: 60pt">Households Making Less Then $40,000</span>'),
       y = "",
       x = "",
       tag = paste('<span style="color: gray20; font-size: 30pt">Roughly 2.3 million New York households, or 31% of state\'s households make less then $40,000 a year.</span><br /><span style="font-size:36pt">&nbsp;</span><br />',
                   'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br />Source: 2021 American Community Survey 5 yr, Table No. B19001.'),
       fill = "")  +
  theme(
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=46, margin=unit(c(20,0,5,0),'pt'), maxheight=0, width = 0.38),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555', width=0.7, valign=0, vjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.key.height = unit(2.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
  ) 

fn <- str_c('households')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


