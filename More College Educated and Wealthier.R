library(tidycensus)
library(tidyverse)

rm(list=ls())


load_variables(2020,'acs5') %>% View()

college_ed <- str_c('B15003_0',seq(22,25))
total_ed <- 'B15003_001'
medincome <- 'B19013_001'


acs <- get_acs(geography = 'block group', 
        variables = c(college_ed,total_ed, medincome),
        year = 2020,
        survey = 'acs5',
        state = 'ny',
        output = 'wide'
)

acsny <- get_acs(geography = 'state', 
                 variables = c(college_ed,total_ed, medincome),
                 year = 2020,
                 survey = 'acs5',
                 state = 'ny',
                 output = 'wide'
)

acsny %>%
  mutate(college = sum(B15003_022E, B15003_023E, B15003_024E, B15003_025E)/B15003_001E,
         median = B19013_001E)
