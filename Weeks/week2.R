library(tidyverse)
library(ggrepel)

options(scipen = 99)

salary_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week4_australian_salary.csv") %>%
  select(-X1)

salary_raw %>%
  group_by(gender) %>%
  nest() %>%
  mutate(percentile = map(data, ~quantile(.$average_taxable_income, probs = seq(0.1, 1.0, by = 0.1)))) %>%
  unnest(map(percentile, broom::tidy)) %>%
  spread(gender, x) %>%
  mutate(
    percent = (Male - Female)/Female,
    percentile = as.numeric(str_replace(names, "%", ""))
  ) %>%
  ggplot(aes(percentile, percent)) + 
  geom_line(size = 1, color = "#F17F42") + 
  geom_point(size = 2, color = "#F17F42") +
  geom_area(alpha = 0.4, fill = "#F17F42") +
  annotate("text", label = "The\nAustralian\nGender\nGap", x = 60, y = 0.13, size = 10, family = "Noto Sans CJK JP", color = "white", fontface = "bold") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_reverse(labels = scales::percent_format(), limits = c(0.7, 0), position = 'right', breaks = seq(0, 0.7, by = 0.1)) + 
  scale_x_continuous(minor_breaks = NULL, labels = function(x) {str_c(as.character(x), "th")}, breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.8), family = "Merriweather"),
    plot.subtitle = element_text(size = rel(1.2), family = "Merriweather Light", margin = margin(0,0,20,0)),
    text = element_text(family = "Noto Sans CJK JP Light"),
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.text = element_text(size = rel(1))
  ) +
  labs(
    x = "Income Percentile within Gender",
    y = "Wage Difference Percent",
    title = "Australia's Income Gap within Gender",
    subtitle = "How much more did men make on average compared to Women\nBy Income Percentile in 2013-14",
    caption = "By Kanishka Misra\nData from: data.gov.au"
  )

ggsave("week2.png", height = 10, width = 9)
