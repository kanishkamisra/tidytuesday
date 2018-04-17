library(tidyverse)
library(readxl)
library(janitor)

mortality_raw <- read_excel("data/global_mortality.xlsx")

mortality <- mortality_raw %>% 
  clean_names() %>%
  gather(cardiovascular_diseases_percent:terrorism_percent, key = "cause", value = "percent") %>%
  mutate(cause = str_replace(cause, "_percent", "")) 

mortality %>%
  group_by(year, cause) %>%
  summarize(world_total = sum(percent)) %>%
  ggplot(aes(year, world_total, color = cause, group = cause)) + 
  geom_line() + 
  scale_y_log10()

mortality %>%
  filter(cause %in% c("cancers", "cardiovascular_diseases")) %>%
  spread(cause, percent) %>%
  mutate(
    colors = case_when(
      country_code %in% c("USA", "IND", "DEU") ~ c("#E53A40"),
      TRUE ~ "black"
    ),
    alpha = case_when(
      country_code %in% c("USA", "IND", "DEU") ~ 1,
      TRUE ~ 3/10
    ),
    label_val = case_when(
      country_code %in% c("USA", "IND", "DEU") ~ as.character(year),
      TRUE ~ ""
    )
  ) %>%
  ggplot(aes(cancers, cardiovascular_diseases, group = country, color = colors, alpha = alpha)) +
  geom_path() + 
  geom_text(aes(label = label_val)) +
  scale_color_identity() +
  scale_alpha_identity()
