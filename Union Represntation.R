library(tidyverse)
library(rvest)
library(sf)
library(ggtext)

un <- read_html('https://www.bls.gov/news.release/union2.t05.htm')

un %>% html_table(header=F) %>% .[[1]] -> unr

unr[1:3,1:11] <- map(unr[1:3,1:11], ~.x %>% na.omit() %>% as.vector() %>% paste(collapse=' '))

unr <- unr %>% janitor::row_to_names(3) %>% janitor::clean_names()
unr <- unr %>% filter(state_state_state != '')
unr <- unr %>% mutate(across(2:ncol(.), ~parse_number(.)))

st <- states(cb=T) %>% shift_geometry() %>% rmapshaper::ms_simplify()

unr %>% inner_join(st, ., by=c('NAME'='state_state_state')) %>%
  ggplot() + geom_sf(aes(fill=x2022_membersof_unions_1_percentofemployed), linewidth=1, color='gray90') +
  ggsflabel::geom_sf_text_repel(aes(label=str_c(STUSPS,'\n',scales::comma(x2022_membersof_unions_1_percentofemployed,accuracy=0.1),'%')), point.size=NA, bg.color='white', bg.r=0.2) +
  scale_fill_viridis_c(option='E', direction = -1) +
  coord_sf(expand=F) +
  theme_void()+
  labs(title = str_c('<span style="font-size: 38pt; color: gray10">2022 Percentage of Workers Who are Union Members</span>'),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'<br />Source: Bureau of Labor Statistics, Union Membership'),
       fill = "")  +
  theme(
    legend.position = 'None',
    text= element_text(family='Roboto Condensed',size=14, color='black'),
    plot.title=element_textbox(halign = 0.5, hjust=0.5, face='bold',size=25),
    plot.background = element_rect(fill = "gray90", color="gray90"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_textbox(size=10, hjust=1, height=0, halign=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1.0,0.01),
  ) 

fn <- str_c('union')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))  

