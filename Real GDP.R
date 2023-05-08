library(tidyverse)

read_csv('~/Downloads/A191RL1Q225SBEA.csv') %>%
  mutate(pos = A191RL1Q225SBEA>0) %>%
  ggplot(aes(DATE, A191RL1Q225SBEA, fill=pos)) +
  geom_col() +
  coord_cartesian(ylim=c(-8,8), expand=F) +
  scale_y_continuous(breaks=seq(-8,8), labels = scales::label_percent(scale=1), name='') +
  scale_x_date(labels = function(x) zoo::format.yearqtr(x, "Q%q %Y"), date_breaks = '3 months', date_minor_breaks = '3 months', name='') +
  theme_minimal() +
  labs(
    title = 'Real Gross Domestic Product, 2013-2023',
    caption = 'Graph by Andy Arthur. Limits applied at -8 and 8 due to drop and spike during the panademic.\nU.S. Bureau of Economic Analysis, Real Gross Domestic Product [A191RL1Q225SBEA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/A191RL1Q225SBEA, April 27, 2023.'
    ) +
  theme (
    text = element_text(family='Roboto Condensed'),
    axis.text.x = element_text(angle=90),
    legend.position = 'none',
    plot.title = element_text(size=20)
  )

ggsave('/tmp/realgdp.jpg', width=11, dpi=150)


library(plotly)


ggplotly() 


ggplotly() %>% plotly_json(FALSE) %>% str_replace_all("\n",'') %>% write('/tmp/gdp.jsx')
