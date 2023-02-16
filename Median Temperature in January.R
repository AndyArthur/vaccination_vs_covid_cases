library(readr)
library(tidyverse)

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


climate <- read_csv("https://www.ncdc.noaa.gov/cag/county/time-series/NY-001-tavg-1-1-1895-2022.csv?base_prd=true&begbaseyear=1895&endbaseyear=2023", 
                                       col_types = cols(Date = col_date(format = "%Y%m")), 
                                       skip = 4)

month = 'January'

ggplot(climate, aes(x=Date, y=Value)) + geom_segment(aes(xend=Date, 
                                                                                        yend=median(climate$Value),
                                                                               color=climate$Value), size=3, show.legend = F) +
  scale_color_gradient2(high='red',low='blue', mid='gray', midpoint = median(climate$Value)) +
  geom_hline(yintercept = median(climate$Value)) + labs(y='Median Temperature in January (F)', x='Year') +
  scale_y_continuous(breaks=seq(0,100,2)) +
  scale_x_date(breaks=seq(as.Date("1810-01-01"), as.Date("2050-12-31"), by="10 years"), labels=scales::date_format("%Y"), expand=c(.01,.01)) +
    geom_smooth(method = 'lm', level=F, alpha=0.3, color='orange') + theme_bw() +
 annotate('text', x=as.Date('1900-01-01'),y=Inf, hjust=0, vjust=1, label=get_formula(lm(Value ~ Date, data=climate))[1] ) +
  labs(title=paste('Median',month,'Temperature for Albany'),
       subtitle=paste('In recent years, Albany\'s',month,'temperature has risen noticably above the historical average of',round(mean(climate$Value),1),'degrees, though last year was an exception...'),
       caption='Data Sources: ncdc.noaa.gov',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%-y")) 
  ) +
  theme_bw() + theme(text=element_text(family='Roboto',size=14), 
                     plot.title=element_text(hjust=0.5, face='bold',size=20), 
                     plot.subtitle=element_text(hjust=0.5),
                     axis.text = element_text(family='Roboto'),
                     plot.tag=element_text(size=10,hjust=0),
                     plot.tag.position = c(0.0,0.01),
                     panel.spacing=unit(c(0,0,0,0),'lines'),
                     panel.border =element_blank(),
                     axis.line = element_line(size=0.5)
  ) 

ggsave(paste('/tmp/median-jan-temp.svg',sep=''), width=1920, height=1080, units='px', dpi=150, device = grDevices::svg)
ggsave(paste('/tmp/median-jan-temp.jpg',sep=''), width=1920, height=1080, units='px', dpi=150)
