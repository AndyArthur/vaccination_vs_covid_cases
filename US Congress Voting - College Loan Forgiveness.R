library(tidyverse)
library(tigris)
library(sf)
library(xml2)
library(lubridate)

rm(list=ls())

# download daily kos shapefile from here
# https://www.dailykos.com/stories/2022/9/21/1742660/-The-ultimate-Daily-Kos-Elections-guide-to-all-of-our-data-sets

unzip('~/Downloads/HexCDv30wm.zip', exdir='/tmp/')
cd <- read_sf('/tmp/HexCDv30wm/HexCDv30wm.shp') %>%
  mutate(Code = str_c(STATEAB, '-', ifelse(STATEAB == CDLABEL, 'AL', str_pad(CDLABEL, 2, 'left', '0'))))

cd <- cd %>% mutate(Code = str_replace(Code, '-',''), 
                    Code = str_replace(Code, 'AL$', '00'))
# change the roll call vote number below
roll.call.vote <- download_xml('https://clerk.house.gov/evs/2023/roll234.xml') %>% read_xml
member.db <- download_xml('https://clerk.house.gov/xml/lists/MemberData.xml') %>% read_xml

members = tibble(
  nameID =  member.db %>% xml_find_all('members//*/bioguideID') %>% xml_text(),
  districtID = member.db %>% xml_find_all('members//*/statedistrict') %>% xml_text(),
  caucusID = member.db %>% xml_find_all('members//*/caucus') %>% xml_text()
)

reso <- roll.call.vote %>% xml_find_all('*/legis-num') %>% xml_text()
reso.name <- roll.call.vote %>% xml_find_all('*/vote-desc') %>% xml_text()
reso.date <- roll.call.vote %>% xml_find_all('*/action-date') %>% xml_text() %>%
  dmy()
  
  

votes = tibble(
  nameID = roll.call.vote %>% xml_find_all('*/recorded-vote/legislator') %>% xml_attr('name-id'),
  vote = roll.call.vote %>% xml_find_all('*/recorded-vote/vote') %>% xml_text()
)

votes <- votes %>%
  group_by(vote) %>%
  mutate(vote = str_c(vote, ' - ',n(),''))

usst <- states(cb=T, resolution = '20m') %>% shift_geometry() %>% filter(GEOID < 57)

cd %>% 
  inner_join(members, join_by(Code == districtID)) %>% 
  inner_join(votes) %>%
  ggplot() + geom_sf(aes(fill=vote),linewidth=0.2, color='white') +
  geom_sf_text(aes(label=CDLABEL, color=vote), size=2, fontface='bold') +
  scale_fill_manual(values=c('#ff3333','gray90', 'lightgreen')) +
  scale_color_manual(values=c('white','gray20','gray20')) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 22pt; font-weight: bold"><b>',reso.name,'</b></span> (', reso
                     ,' - ',format(reso.date,'%b %d, %Y'),')'
  ),
  tag=str_c(
    '<b>Data Source:</b> 2023 US House Clerk, DailyKos Congressional Hexmap Shapefile<br />',
    'Andy Arthur - ', format(Sys.Date(), format="%-m.%-d.%y"),
    ''),
  fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=18),
    plot.title=ggtext::element_textbox_simple(hjust=0, halign=0, size=18, margin=margin(5,0,20,0), maxheight = 0, maxwidth=0.7),
    plot.background = element_rect(fill = "white", linewidth=0),
    plot.tag=ggtext::element_textbox(size=14,hjust=0, color='#555555', maxheight=0, halign = 0, valign=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0),    
    legend.key.height = unit(2,'cm'),
    legend.key.width = unit(1,'cm'),
    legend.text = element_text(margin = margin(t = 18, unit = "pt")),
    legend.position = c(1,0.4),
    legend.justification = c(1,0.5)
  ) +
  guides(fill=guide_legend(reverse=T), color=guide_none())

fn <- str_c('college-loan-forgiveness')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1900, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1900, height=1200, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

