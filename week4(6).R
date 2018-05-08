# Import Libraries
library(tidyverse)
library(readxl)
library(janitor)
library(geofacet)

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
  count(state, sheet) %>%
  ggplot(aes(sheet, n, fill = sheet)) +
  geom_col() +
  coord_flip() + 
  facet_geo(~state, grid = "us_state_grid4")
  
  
  
