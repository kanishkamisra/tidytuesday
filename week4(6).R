# Import Libraries
library(tidyverse)
library(readxl)
library(janitor)
library(geofacet)
library(ggbeeswarm)

coffee_raw <- data_frame(
  sheet = c(1, 2, 3)
) %>%
  mutate(
    data = map(sheet, function(x){
      read_excel("data/week6_coffee_chains.xlsx", sheet = x)
    })
  )

coffee_clean <- coffee_raw %>%
  mutate(
    data = map(data, function(x){
      x %>%
        clean_names()
    })
  ) %>%
  mutate(
    data = map(data, function(x) {
      x %>%
        select_at(vars(matches("city|state|country"))) %>%
        rename_at(vars(matches("city|state|country")), funs(str_replace(str_replace(., "_province", ""), "e_", "")))
    })
  ) %>%
  mutate(
    sheet = case_when(
      sheet == 1 ~ "Starbucks",
      sheet == 2 ~ "Tim Hortons",
      sheet == 3 ~ "Dunkin' Donuts"
    )
  ) %>%
  unnest()

coffee_clean %>%
  mutate(country = toupper(str_replace(country, "USA", "US"))) %>%
  filter(country == "US") %>%
  filter(sheet != "Tim Hortons") %>%
  count(city, state, sheet) %>%
  spread(sheet, n, fill = 0) %>%
  rename(dunkin_donuts = `Dunkin' Donuts`) %>%
  ggplot(aes(dunkin_donuts, Starbucks)) +
  geom_jitter(size = 2, alpha = 0.3) +
  facet_geo(~state) +
  scale_x_log10() +
  scale_y_log10()

top_states <- coffee_clean %>%
  mutate(country = toupper(str_replace(country, "USA", "US"))) %>%
  filter(country == "US") %>%
  filter(sheet != "Tim Hortons") %>%
  count(state) %>%
  top_n(10) %>%
  arrange(n)

set.seed(322)

coffee_clean %>%
  mutate(country = toupper(str_replace(country, "USA", "US"))) %>%
  filter(country == "US") %>%
  filter(sheet != "Tim Hortons") %>%
  count(state, city, sheet) %>%
  filter(state %in% top_states$state) %>%
  ggplot(aes(state, n, color = sheet)) +
  # geom_jitter() +
  geom_quasirandom(method = "tukey", size = 0.75) +
  # geom_density_ridges() +
  coord_flip() +
  scale_color_manual(values = c("#ef5285", "#548687")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.8), family = "Merriweather"),
    plot.subtitle = element_text(size = rel(1.2), family = "Merriweather Light", margin = margin(0,0,20,0)),
    text = element_text(family = "Noto Sans CJK JP Light"),
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    # axis.text = element_text(size = rel(0.9)),
    legend.position = "top"
  ) + 
  guides(color = guide_legend(override.aes = list(size = 4))) +
  labs(
    color = "Coffee Chain",
    x = "State",
    y = "Count",
    title = "Mr. Stark I don't feel so good :(",
    subtitle = "Starbucks and Dunkin' Donuts in the top 10 states with coffee shops in the USA",
    caption = "By Kanishka Misra\nData from: Kaggle and odditysoftware.com"
  )

ggsave("week4(6)_a.png", height = 9, width = 11)  




  
