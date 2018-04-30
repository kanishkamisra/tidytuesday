library(tidyverse)
library(geofacet)
library(ggridges)

opsions(scipen = 99)

census_2015 <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/acs2015_county_data.csv")

census_2015 %>%
  gather(Professional:Production, Unemployment, key = "Employment", value = "percent") %>%
  ggplot(aes(percent, Employment, fill = Employment, color = Employment)) + 
  geom_density_ridges() +
  facet_geo(~State, grid = "us_state_grid4") +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_brewer(palette = "Spectral") +
  scale_x_continuous(labels = function(x) {str_c(as.character(x), "%")}) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.8), family = "Merriweather"),
    plot.subtitle = element_text(size = rel(1.2), family = "Merriweather Light", margin = margin(0,0,20,0)),
    text = element_text(family = "Noto Sans CJK JP Light"),
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    # axis.text = element_text(size = rel(0.9)),
    legend.position = "top"
  ) + 
  guides(fill = guide_legend(ncol = 6, byrow = T)) +
  labs(
    title = "Employment Distribution in the United States",
    subtitle = "American Community Survey (2015) 5-year Estimates",
    x = "Percent of Population in State",
    caption = "By Kanishka Misra\nData from: Kaggle"
  )

ggsave("week3.png", height = 12, width = 17)  
