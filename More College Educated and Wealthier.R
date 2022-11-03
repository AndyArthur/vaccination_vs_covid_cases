library(tidycensus)
library(tidyverse)

rm(list=ls())


load_variables(2020,'acs5') %>% View()

college_ed <- str_c('B15003_0',seq(22,25))
total_ed <- 'B15003_001'
medincome <- 'B19013_001'


acs <- get_acs(geography = 'county subdivision', 
        variables = c(college_ed,total_ed, medincome),
        year = 2020,
        survey = 'acs5',
        state = 'ny',
        output = 'wide',
        geometry = T
) %>% rmapshaper::ms_simplify()


cls <- colorspace::qualitative_hcl(palette = 'Set 2', n=6)

nyco <- counties(cb=T, 'ny') %>% rmapshaper::ms_simplify()
nycos <- county_subdivisions(cb=T, 'ny') %>% rmapshaper::ms_simplify()
acs %>%
  mutate(college = sum(B15003_022E, B15003_023E, B15003_024E, B15003_025E)/B15003_001E,
         median = B19013_001E,
         college.rank = cume_dist(college),
         median.rank = cume_dist(median),
         type = ifelse(college.rank < .1, 'Most Educated', NA),
         type = ifelse(median.rank > .9, 'Most Wealthy', type),
         type = ifelse(college.rank > .9, 'Least Educated', type),
         type = ifelse(median.rank < .1, 'Least Wealthy', type),
         type = ifelse(college.rank < .1 & median.rank > .9, 'Most Educated \nand Wealthy', type),
         type = ifelse(college.rank > .9 & median.rank < .1, 'Least Educated \nand Wealthy', type),
         
         type = factor(type, levels=c('Least Educated', 'Least Wealthy', 'Least Educated \nand Wealthy', 'Most Educated', 'Most Wealthy','Most Educated \nand Wealthy'))
         ) %>%
      drop_na(type) %>%
  ggplot() +
  geom_sf(aes(fill=type), size=0) +
  scale_fill_brewer(palette = 'Set2') +
  geom_sf(data=nyco, fill=NA) +
  geom_sf(data=nycos, fill=NA, size=0.1) +
  theme_void() +
  coord_sf(expand=F, crs=3857) +
  labs(title = str_c('<span style="font-size: 60px; color:',cls[1],'">Most </span> and ',
                     '<span style="font-size: 60px; color:',cls[2],'">Least </span> ',
                     '<span style="font-size: 60px; color:',cls[3],'">College Educated </span> and ',
                     '<span style="font-size: 60px; color:',cls[5],'">Wealthy </span>',
                     '<span style="font-size: 60px; color:',cls[6],'">Municipalities </span><br /><br />Bottom and Top 10<br />Percent of Municipalities'
                      ),
       y = "",
       x = "",
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: 2020 5-Yr ACS Community Survey'),
       fill = "")  +
  theme(
    legend.key.height = unit(1.5,'cm'),
    legend.key.width = unit(1,'cm'),
    legend.position = c(0.3,0.17),
    legend.justification = 'center',
    legend.spacing.y = unit(0.5, 'cm'),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=20, margin=unit(c(25,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) +
  guides(fill = guide_legend(byrow = TRUE, direction = 'horizontal') )



fn <- str_c('least-most-college')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg, bg='white')
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg, bg='white')
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))


