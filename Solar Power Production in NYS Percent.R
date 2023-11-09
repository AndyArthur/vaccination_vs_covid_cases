key <- 'EokzGbTAEYJjstg4LZSLp7BC03lZQtd5wyHvJ1eH'

library(tidyverse)
library(eia)
library(janitor)
eia_set_key(key)

d<-eia_data('ELEC.GEN.ALL-NY-99.M;ELEC.GEN.SPV-NY-99.M;ELEC.GEN.DPV-NY-99.M')

unnest(d, cols = data) %>%
  separate(col = name, into=c(NA,'type'), sep=' : ')  %>% select(date, type, value) %>% pivot_wider(names_from = type, values_from = value) %>% clean_names() %>% 
  mutate(large_solar = (utility_scale_photovoltaic/all_fuels)*100, 
         small_solar = (small_scale_solar_photovoltaic/all_fuels)*100, total_solar = large_solar + small_solar) %>% 
  select(date, large_solar, small_solar, total_solar) %>% pivot_longer(cols = c(large_solar, small_solar, total_solar)) %>% 
  mutate(linetype = ifelse(name == 'total_solar', 'solid', 'dashed')) %>%
  ggplot(aes(x=date, y=value, col=forcats::fct_rev(name))) + geom_line(aes( linetype=linetype), size=1,) +
  scale_y_continuous(expand=c(0,0), labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_x_date(date_breaks = '6 month', date_minor_breaks = 'month', date_labels = '%b\n%Y', expand=c(0,0)) +
  scale_linetype_identity() +
  coord_cartesian(xlim=(c(as.Date('2012-01-01'), NA))) +
  scale_color_manual(values=c('red','orange','yellow'), name='', labels=c('Total', 'Small-Scale', 'Large-Scale')) +
  labs(title ='How much does solar energy contribute to \nNew York power production?',
       #subtitle='Solid line represents actual production, while smoothed line represents loess smoothed growth curve.',
       y = "",
       x = "",
       caption='Energy Information Agency, series: ELEC.GEN.ALL-NY-99.M;ELEC.GEN.SPV-NY-99.M;ELEC.GEN.DPV-NY-99.M',
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
       fill = "") +
  theme_minimal() +
  theme(
    text=element_text(family='Overpass',size=14),
    plot.title=element_text(hjust=0.5, face='bold',size=28),
    panel.grid.major=element_line(size=.3, color='darkgray'),
    panel.grid.minor=element_line(size=.1, color='darkgray'),    
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_text(hjust=0.5),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.caption=element_text(size=10, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.position = 'top',
  ) 

file <- 'Solar Power as Percent of Grid Output-rev'
ggsave(paste('Desktop/',file,'.jpg',sep=''), width=1920, height=1600, units='px', dpi=150)
ggsave(paste('Desktop/',file,'.svg',sep=''), width=1920, height=1600, units='px', dpi=150, device = grDevices::svg)
