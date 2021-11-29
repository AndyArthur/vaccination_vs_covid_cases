library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(lubridate)

vac <- jsonlite::fromJSON('https://data.cdc.gov/resource/8xkx-amqh.json?recip_state=NY')
cv <- read.csv(url("https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD"))

vac$series_complete_12pluspop = as.numeric(vac$series_complete_12pluspop)

cv$Test.Date = as.Date(cv$Test.Date,format='%m/%d/%Y')
cv$Pos.Rate = (cv$New.Positives/cv$Total.Number.of.Tests.Performed)*100

ravg <- cv %>% filter(Test.Date > today() %m-% days(7) ) %>% group_by(County) %>% summarize(avg=mean(Pos.Rate))
vac <- vac %>% filter(`date` == max(vac$date)) %>% mutate(County = str_replace(recip_county, " County", ""))

comb <- left_join(ravg,vac, by="County")

ggplot(comb, aes(y=avg,x=series_complete_12pluspop, label=County)) + 
  geom_smooth(method="lm",level=0.95, alpha=0.2, color='gold', fill='gold') +
  geom_point(size=2, alpha=0.5, color='blue') + 
  labs(title='COVID-19 Positivity vs. Vaccination Rate in New York',
       subtitle='Counties with higher vaccination rates generally have fewer positive COVID-19 tests.',
       caption='Data Sources: data.cdc.gov and health.data.ny.gov',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%m/%d/%y")) 
  ) +
  geom_text_repel(size=5, max.overlaps = 15, family='Overpass Mono') +
  scale_x_continuous(name=paste('Vaccination Rate, Age 12+ (CDC, ',format(as.Date(max(vac$date)), format="%m/%d"),')', sep="")) +
  scale_y_continuous(name=paste('Average of Positivity Rate (NYSDOH, ',format(today() %m-% days(7), format="%m/%d - "), format(today(), format="%m/%d)"), sep=""))  +
  theme_bw() + theme(text=element_text(family='Overpass',size=14), 
                     plot.title=element_text(hjust=0.5, face='bold',size=20), 
                     plot.subtitle=element_text(hjust=0.5),
                     plot.tag=element_text(size=10,hjust=0),
                     plot.tag.position = c(0.0,0.01),
                     panel.spacing=unit(c(0,0,0,0),'lines'),
                     panel.border =element_blank(),
                     axis.line = element_line(size=0.5)
  )

ggsave(paste('/tmp/covid-vacc-rate.svg',sep=''), width=1920, height=1080, units='px', dpi=150, device = grDevices::svg)
ggsave(paste('/tmp/covid-vacc-rate.jpg',sep=''), width=1920, height=1080, units='px', dpi=150)

