library(tidyverse)
library(reticulate)
library(sf)
library(ggtext)

ueArea <- read_sf('/home/andy/NYSDOL Unemployment Areas.geojson')

py$df %>% 
  filter(MONTH == 4 & (YEAR == 2022 | YEAR == 2012)) %>%
  select(-DATETIME, -UNEMPRATE) %>%
  pivot_wider(names_from = YEAR, values_from = 4:6) %>%
  mutate(empChg = ((EMP_2022-EMP_2012)/EMP_2012)*100) %>% 
  left_join(ueArea, ., by=c('NAMELSAD10'='AREA')) %>% 
  write_sf('/tmp/10yearsjobs.gpkg')
            

py$df %>% 
  filter(MONTH == 4 & (YEAR == 2022 | YEAR == 2012)) %>%
  select(-DATETIME, -UNEMPRATE) %>%
  pivot_wider(names_from = YEAR, values_from = 4:6) %>%
  filter(grepl('County',AREA)) %>%
  mutate(empChg = ((EMP_2022-EMP_2012)/EMP_2012)*100, 
         AREA = str_replace(AREA, ' County',''),
         growth = ifelse(empChg>0, 'Growth','Decline'),
         ) %>%
  ggplot(aes(y=reorder(AREA, empChg), x=empChg, fill=growth)) + geom_col() +
  labs(title = 'Percent <span style="color: darkgreen">Change in Jobs</span>, April 2022 vs April 2012',
       y = "County",
       x = "Percent Change in Number of Employed Persons",
       caption='NYS Department of Labor',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_minimal() +
  theme(
    text= element_text(family='Noto Sans',size=12),
    plot.title=element_textbox(hjust=0.5, face='bold',size=28),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5),
    axis.text  = element_text(size=10),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'none',
  )

fn <- str_c('chg-in-jobs')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1080, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

