library(tidyverse)
library(ggrepel)

options(scipen = 99)

life_expectancy <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week14_global_life_expectancy.csv")

since_1950 <- life_expectancy %>%
  filter(!is.na(code), year >= 1950) %>%
  inner_join(
    life_expectancy %>%
      filter(year == 1950) %>%
      select(country, life_1950 = life_expectancy)
  ) %>%
  mutate(
    since_1950 = life_expectancy - life_1950
  )

rwanda_cambodia <- since_1950 %>%
  filter(country %in% c("Rwanda", "Cambodia")) %>%
  group_by(country) %>%
  mutate(since_1950 = min(since_1950)) %>%
  ungroup() %>%
  filter(life_expectancy - life_1950 == since_1950) %>%
  mutate(
    color = case_when(
      country == "Rwanda" ~ "#ef9e9f",
      country == "Cambodia" ~ "#f26d5b",
      TRUE ~ "black"
    )
  )

set.seed(1776)

since_1950 %>%
  mutate(
    color = case_when(
      country == "Rwanda" ~ "#ef9e9f",
      country == "Cambodia" ~ "#f26d5b",
      TRUE ~ "black"
    ),
    alpha = case_when(
      country %in% c("Rwanda", "Cambodia") ~ 1,
      TRUE ~ 0.1
    ),
    size = case_when(
      country %in% c("Rwanda", "Cambodia") ~ 1.2,
      TRUE ~ 0.3
    )
  ) %>%
  ggplot() + 
  geom_line(aes(year, since_1950, group = country, color = color, alpha = alpha, size = size)) +
  scale_alpha_identity() +
  scale_size_identity() +
  scale_x_continuous(breaks = seq(1950, 2015, by = 5)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-30, 45, by = 5), limits = c(-30, 45)) +
  geom_text_repel(
    data = rwanda_cambodia, 
    aes(year, since_1950, label = glue::glue('{country}\n{round(life_expectancy, 2)} years in {year}'), 
        color = color), 
    nudge_y = -2, 
    size = 4, 
    fontface= 'bold', 
    family = "Roboto"
  ) +
  scale_color_identity() +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = rel(1.8), family = "Merriweather"),
    plot.subtitle = element_text(size = rel(1.2), family = "Merriweather Light", margin = margin(0,0,20,0)),
    text = element_text(family = "Noto Sans CJK JP Light"),
    axis.text = element_text(size = rel(0.9)),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + 
  labs(
    title = "Recovery from Genocide",
    subtitle = "Life expectancy changes since 1950 in Rwanda and Cambodia\nas compared to other countries",
    y = "Change in Life expectancy since 1950 (in years)",
    x = "Year",
    caption = "Made by Kanishka Misra\nData from Our World in Data"
  )

ggsave("week 14.png", height = 9, width = 8)
