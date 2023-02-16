library(rvest)
library(tidyverse)
library(xml2)
library(RSelenium)
library(netstat)

rm(list = ls())

system("killall java")

system('find ~/.local -name LICENSE.chromedriver | xargs rm')

rs <- rsDriver(
  remoteServerAddr = "localhost",
  port = free_port(random = T),
  browser = "firefox",
  # extraCapabilities = list("moz:firefoxOptions" = list(args = list("--headless"))),
  verbose = F
)

rsc <- rs$client
rsc$navigate("https://seethroughny.net/payrolls")

# stop and grab details you want, then load all records until button disappears
# or limit to 20 records so don't accidentially pull the universe
for (i in seq(1,30)) {
  rsc$findElement(using='css', '#data_loader')$clickElement()
  
  if (rsc$findElement(using='css', '#data_loader')$getElementAttribute('style')[[1]] == 'display: none;')
    break;
  
  Sys.sleep(0.2)
}


rsc$getPageSource() %>%
  unlist() %>%
  read_html() %>%
  html_table() %>%
  .[[1]] %>% 
  janitor::clean_names() -> employees

employees %>%
  filter(row_number() %% 2 == 0) %>%
  select(name) %>%
  separate(name, sep='\n', into=c(NA,'subagency',NA,NA,NA,'title',NA,NA,NA,'rateofpay',NA,NA,NA,'payyear',NA,NA,NA,'paybasis',NA,NA,NA,'branch') ) %>%
  cbind(employees %>% filter(row_number() %% 2 != 0), .) %>%
  mutate(across(everything(), str_trim),
         total_pay = parse_number(total_pay)) %>%
  select(-x, -x_2, -subagency_type) -> employees

write_csv(employees, '/tmp/employee.csv')
