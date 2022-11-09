library(tidyverse)
library(rvest)
library(janitor)

index_pg <- read_html('https://enr.boenyc.gov/CD24697AD0.html') 
pages <- index_pg %>% html_nodes("a") %>% html_attr("href")
ads <- index_pg %>% html_nodes("a") %>% html_text() %>% parse_number()


if(exists('results')) rm(results)
for (i in 3:(length(pages)-1)) {

  dt <- read_html(str_c('https://enr.boenyc.gov/',pages[i])) %>% html_table() %>% .[[3]]
  
  results <- rbind(if(exists('results')) results, 
                   cbind(
                    ad = rep(ads[i], nrow(dt)),
                    dt
                   )

  )
  Sys.sleep(0.4)
}

results %>% row_to_names(2) %>% 
  clean_names() %>%
  remove_empty() %>%
  rename(ad = x23, ed = x) %>%
  select(-ncol(.)) %>%
  mutate(across(where(is.character),  ~parse_number(.))) %>%
  drop_na(ed) %>%
  mutate(
    total = democratic + republican + conservative + working_families,
    ElectDist = str_c(ad, str_pad(ed,width = 3, side = 'left', pad = '0')) %>%
           as.numeric())  -> cleaned_results

nyc_eds <- arcpullr::get_spatial_layer('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Election_Districts/FeatureServer/0/')

nyco <- tigris::counties('ny', cb = F) %>% filter(NAME %in% c('Bronx','New York','Kings','Queens','Richmond'))

nyc_eds_simple <- nyc_eds %>% rmapshaper::ms_simplify()

nyc_eds_simple %>% inner_join(cleaned_results, by='ElectDist') %>%
  ggplot() + geom_sf(aes(fill=(democratic+working_families)/total), size=0) +
  geom_sf(data=nyco, fill=NA, color='white') +
  ggredist::scale_fill_party_c() + theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 35pt">2022 Governor Race<br /><span style="color: darkblue">Kathy Hochul</span> ',
                     'vs. <span style="color: darkred">Lee Zeldin</span></span> in NY City'),
       tag=paste('NYC Board of Elections - Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),)  +
  theme(
    text= element_text(family='Roboto Condensed',size=14),
    plot.title=ggtext::element_textbox_simple( face='bold', size=20, margin=margin(10, 0,0,0), halign=0.5, hjust=0.5),
    plot.background = element_rect(fill = "snow", color="snow"),
    plot.tag=ggtext::element_textbox(size=12,hjust=1, color='#555555', maxheight=0, halign = 1, valign = 0, margin=margin(5,5,5,5)),
    plot.tag.position = c(1,0),
    legend.position = c(0.12, 0.7),
    legend.justification = 'right',
    legend.key.height = unit(2,'cm'),
    legend.key.width = unit(0.3,'cm'),
    legend.text = element_text(margin = margin(t = 30, unit = "pt")),
  ) 

fn <- str_c('nyc-gov-race')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=130, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

