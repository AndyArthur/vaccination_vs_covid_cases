library(RSelenium)
library(tidyverse)
library(netstat)
library(rvest)
library(lubridate)
library("googlesheets4")

rm(list=ls())

system('killall java')

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

#gs4_auth()
url <- 'https://docs.google.com/spreadsheets/d/1edIyTy1jE90ml_lFuFQup-LPSqtv1oeVJnGg7X1Pics/edit#gid=0'

while(TRUE) {
  types <- data.frame(
   opt = c(6, 7, 8),
   type = c('US Congress', 'State Senate', 'Assembly')
  )
  
  for (i in 1:3) {
    # contest
    rsc$findElement(using='xpath', str_c('/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[1]/select/option[',types[i,'opt'],']'))$clickElement()
    
    # districts 
    rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[2]/select/option[1]')$clickElement()
    
    # county path
    rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[3]/select/option[1]')$clickElement()
    
    # click search
    rsc$findElement(using='xpath','//*[@id="ctl00_HomePage_btnGo"]')$clickElement()
    
    html <- rsc$getPageSource() %>% unlist() %>% read_html()
    tables <- html_table(html)
    
    lastUpdate <- tables[[4]][1,1] %>% as.character()
    
    bc <- NA
    for (table in tables) {
      if (!grepl('\\>',table[[1,1]]) | !grepl('Reporting',table[[1,1]])) next
      
      ad <- str_match(table[[1,1]],'(.*?) >') %>% .[,2]
      
      rpt <- str_match(table[[1,1]],'Reporting: (.*?) of') %>% .[,2] %>% parse_number()
      te <- str_match(table[[1,1]],' of (.*?)$') %>% .[,2] %>% parse_number()
      
      table %>% 
        janitor::row_to_names(3) %>% 
        janitor::clean_names() %>%
        .[-nrow(.),c(-3,-6,-9, -10)] %>%
        mutate(
          across(3:6, ~parse_number(.)),
        ) -> table
      
    
      table <- cbind(district = parse_number(ad), 
                     table, 
                     reported_eds = rep(rpt, nrow(table)),
                     total_eds = rep(te, nrow(table)),
                     last_updated = rep(lastUpdate, nrow(table)))
      
      bc <- rbind(bc, table)  
      
    }
    
    bc <- bc[-1,]
    write_sheet(data = bc, ss = url, sheet = types[i,'type'])
    Sys.sleep(30)
  }
  
  types <- data.frame(
    opt = 1:4,
    type = c('Governor','Comptroller', 'Attorney General', 'US Senator')
  )
  
  for (i in 1:4) {
    # contest
    rsc$findElement(using='xpath', str_c('/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[1]/select/option[',types[i,'opt'],']'))$clickElement()
    
    # districts 
    rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[2]/select/option[1]')$clickElement()
    
    # county path
    rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[3]/select/option[2]')$clickElement()
    
    # click search
    rsc$findElement(using='xpath','//*[@id="ctl00_HomePage_btnGo"]')$clickElement()
    
    html <- rsc$getPageSource() %>% unlist() %>% read_html()
    tables <- html_table(html)
    
    lastUpdate <- tables[[4]][1,1] %>% 
      as.character() %>% 
      as_datetime(format='%m/%d/%Y  %H:%M:%S %p', tz='EST')
    
    ac <- NA
    for (table in tables) {
      
      if (!grepl('\\>',table[[1,1]]) | !grepl('Reporting',table[[1,1]]) |
          grepl('All Counties',table[[1,1]])) next
      
      race <- str_match(table[[1,1]],'(.*?) >') %>% .[,2]
      cty <- str_match(table[[1,1]],'> (.*?) \\(Active') %>% .[,2]
      rpt <- str_match(table[[1,1]],'Reporting: (.*?) of') %>% .[,2] %>% parse_number()
      te <- str_match(table[[1,1]],' of (.*?)$') %>% .[,2] %>% parse_number()
      
      table %>% 
        janitor::row_to_names(3) %>% janitor::clean_names() %>% .[-nrow(.),] %>% .[,c(1,2,4,5,7,8)]  -> table
      
      table %>% mutate(across(3:6, ~parse_number(.))) -> table
      
      table <- cbind(table, Race=rep(race,nrow(table)), County=rep(cty,nrow(table)),  Reporting=rep(rpt,nrow(table)), Total=rep(te,nrow(table)),
                     Updated = rep(lastUpdate,nrow(table))
      )
      ac <- rbind(ac, table)  
      
    }
    
    ac <-ac[-1,]
    ac %>% pivot_wider(names_from = c(candidate,party), values_from = c(percent_by_party, votes_by_party), id_cols = c(County, Reporting, Total), names_sep = " ") -> cc
    
    cc <- cbind(cc, updated=rep(lastUpdate,nrow(cc)))
    
    write_sheet(data = cc, ss = url, sheet = types[i,'type'])
    Sys.sleep(30)
  }
}
