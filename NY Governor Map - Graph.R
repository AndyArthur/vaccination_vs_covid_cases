library(RSelenium)
library(tidyverse)
library(netstat)
library(rvest)
library(geofacet)
library(ggtext)
library(lubridate)

rm(list=ls())

rs <- rsDriver(
  remoteServerAddr='localhost',
  port = free_port(random=T),
  browser = 'firefox',
  extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless'))),
  verbose = F
)

rsc <- rs$client
rsc$navigate("https://nyenr.elections.ny.gov/")

Sys.sleep(0.5)

# contest
rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[1]/select/option[1]')$clickElement()

# districts 
rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[2]/select/option[1]')$clickElement()

# county path
rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[3]/select/option[2]')$clickElement()

# click search
rsc$findElement(using='xpath','//*[@id="ctl00_HomePage_btnGo"]')$clickElement()


html <- rsc$getPageSource() %>% unlist() %>% read_html()

tables <- html_table(html)

ac <- NA
for (table in tables) {
  
  if (!grepl('\\>',table[[1,1]]) | !grepl('Reporting',table[[1,1]]) |
      grepl('All Counties',table[[1,1]])) next
  
  race <- str_match(table[[1,1]],'(.*?) >') %>% .[,2]
  cty <- str_match(table[[1,1]],'> (.*?) \\(Active') %>% .[,2]
  rpt <- str_match(table[[1,1]],'Reporting: (.*?) of') %>% .[,2] %>% parse_number()
  te <- str_match(table[[1,1]],' of (.*?)$') %>% .[,2] %>% parse_number()
  
  table %>%
    janitor::row_to_names(3) %>% 
    clean_names() %>% 
    filter(percent_by_candidate != '', party !='', candidate != 'Total Votes') %>% 
    select(candidate, party, percent_by_candidate, votes_by_candidate) %>%
    mutate(
      across(3:4,
       ~parse_number(.)      
      )
    ) -> table
  
  table <- cbind(table, County=rep(cty,nrow(table)))
  
  ac <- rbind(ac, table)  
  
}

ac <- ac[-1,]

ac %>%
  mutate(candidate = str_extract(candidate, '(\\w*?) (?=/)'),
         County = str_replace(County, 'St.La','St La')
  ) %>%
  ggplot() +
  geom_col(aes(x=candidate, y=votes_by_candidate, fill=candidate)) +
  scale_fill_manual(values=c('blue','red'))+
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal() +
  facet_geo(~ County, grid = "us_ny_counties_grid1", label = "name") +
  labs(
    title = str_c('<span style="font-size: 48pt; color: gray30">2022 Governor\'s Race</span><br /><br />',
                  '<span style="color: darkblue">Kathy Hochul: ', scales::comma(as.numeric(bc[2,2])), '<br />',
                  '<span style="color: darkred">Lee Zeldin: ', scales::comma(as.numeric(bc[3,2])), ''
    ),
    x = "",
    y = "",
    tag=paste('<em>Source:</em> NYS Board of Elections - Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme(
    text= element_text(family='Roboto Condensed',size=10),
    strip.background = element_rect(fill='snow', color='gray90'),
    strip.text = element_text(size = 10),
    panel.background =  element_rect(fill='snow', color='gray90'),
    plot.title=element_textbox(halign = 1, hjust=0, face='bold',size=25, margin=unit(c(30,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.subtitle=element_text(hjust=0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555', height=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    axis.text.x = element_text(size=8),
    legend.position = 'none'
  ) 


fn <- str_c('gov-race-facet')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1600, units='px', dpi=140, device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1600, units='px', dpi=140, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


ac %>%
  mutate(candidate = str_extract(candidate, '(\\w*?) (?=/)'),
         County = str_replace(County, 'St.La','St La')
  ) %>%
  ggplot() +
  geom_col(aes(x=candidate, y=votes_by_candidate, fill=candidate)) +
  scale_fill_manual(values=c('blue','red'))+
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal() +
  facet_wrap(~ County, nrow = 4) +
  labs(
    title = str_c('<span style="font-size: 38pt; color: gray30">2022 Governor\'s Race</span><br /><br />',
                  '<span style="color: darkblue">Kathy Hochul: ', scales::comma(as.numeric(bc[2,2])), '<br />',
                  '<span style="color: darkred">Lee Zeldin: ', scales::comma(as.numeric(bc[3,2])), ''
    ),
    x = "",
    y = "",
    tag=paste('<em>Source:</em> NYS Board of Elections - Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
    fill = "") +
  theme(
    text= element_text(family='Roboto Condensed',size=10),
    strip.background = element_rect(fill='snow', color='gray90'),
    strip.text = element_text(size = 10),
    panel.background =  element_rect(fill='snow', color='gray90'),
    plot.title=element_textbox(halign = 1, hjust=0, face='bold',size=25, width=1),
    plot.subtitle=element_text(hjust=0.5),
    plot.background = element_rect(fill = "white", color="white"),
    plot.tag=element_textbox(size=10,hjust=0, color='#555555', height=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0),
    axis.text.x = element_text(size=8),
    legend.position = 'none'
  ) 


fn <- str_c('gov-race')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1600, units='px', dpi=140, device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1600, units='px', dpi=140, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


