library(tidyverse)
library(tidycensus)

rm(list=ls())

edu <- get_acs("county", table = "B15003", cache_table = T,
               geometry = F, 
               year = 2020, output = "wide")

pres <- read_csv("https://github.com/tonmcg/US_County_Level_Election_Results_08-20/blob/master/2020_US_County_Level_Presidential_Results.csv")

edu <- edu %>%
  mutate(college = (rowSums(across((matches('B.*?E$') & B15003_022E:B15003_025E)),na.rm=T)/B15003_001E) )

join <- edu %>% select(GEOID, college) %>% left_join(pres, by = c('GEOID'='county_fips'))

get_formula <- function(model) {
  pValue <- paste('p-value=',  format(summary(model)$coefficients[,4][2], scientific=T), sep='')  
  r2Value <- paste('r^2-value=', round(summary(model)$r.squared,4), sep='') 
  
  formula <- broom::tidy(model)[, 1:2] %>%
    mutate(sign = ifelse(sign(estimate) == 1, '+', '-')) %>% #coeff signs
    mutate_if(is.numeric, ~ abs(round(., 7))) %>% #for improving formatting
    mutate(a = ifelse(term == '(Intercept)', paste0('y=', estimate), paste0(sign, estimate, 'x'))) %>%
    summarise(formula = paste(a, collapse = '')) %>%
    as.character 
  
  paste(formula, pValue, r2Value, sep='\n')
}



ggplot(join, aes(x=college,y=per_dem, color=per_dem)) + 
  geom_point(size=1, alpha=0.8) + 
  scale_color_gradient2(midpoint = 0.5, mid = 'gray', low='red', high='blue') +
  scale_y_continuous(name='Percent of Vote for Biden', labels = scales::percent_format()) +
  scale_x_continuous(name='Percent with a Bachelors\' or Post-Graduate Degree', labels = scales::percent_format()) +
  coord_cartesian(expand = F) +
  annotate('text', x=0.80,y=0.05, hjust=1, vjust=0, label=get_formula(lm(college ~ per_dem, data=join))[1] ) +
  labs(title='College Graduation Rate vs 2020 Presidential Race Results',
       caption='Data Sources: Tony McGovern/GitHub and 2020 ACS 5-Yr Edu. Attainment, Age 25+',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")) 
  ) +
  theme_bw() + theme(text=element_text(family='Overpass',size=14), 
                     plot.title=element_text(hjust=0.5, face='bold',size=20), 
                     plot.subtitle=element_text(hjust=0.5),
                     plot.tag=element_text(size=10,hjust=0),
                     plot.tag.position = c(0.0,0.01),
                     panel.spacing=unit(c(0,0,0,0),'lines'),
                     panel.border =element_blank(),
                     axis.line = element_line(size=0.5),
                     legend.position = 'none',
  )

filename <- 'pres_vs_edu'

ggsave(paste('/tmp/',filename,'.svg',sep=''), width=1920, height=1080, units='px', dpi=150, device = grDevices::svg)
ggsave(paste('/tmp/',filename,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=150)

