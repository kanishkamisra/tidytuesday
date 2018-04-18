library(tidyverse)
library(readxl)
library(janitor)
library(ggrepel)

mortality_raw <- read_excel("data/global_mortality.xlsx")

mortality <- mortality_raw %>% 
  clean_names() %>%
  gather(cardiovascular_diseases_percent:terrorism_percent, key = "cause", value = "percent") %>%
  mutate(cause = str_replace(cause, "_percent", ""))

set.seed(111)

countries <- mortality %>%
  distinct(country) %>%
  pull(country)

random_countries <- sample(countries, 6)

mortality %>%
  filter(cause %in% c("cancers", "cardiovascular_diseases")) %>%
  mutate(percent = percent/100) %>%
  spread(cause, percent) %>%
  mutate(
    colors = case_when(
      country %in% random_countries ~ country,
      TRUE ~ "Others"
    ),
    alpha = case_when(
      country %in% random_countries ~ 1,
      TRUE ~ 0.1
    ),
    label_year = case_when(
      year %in% c(seq(1990, 2016, length = 3)) ~ as.character(year),
      TRUE ~ ""
    ),
    label_val = case_when(
      country %in% random_countries ~ label_year,
      TRUE ~ ""
    ),
    path_size = case_when(
      country %in% random_countries ~ 1,
      TRUE ~ 0.75
    ),
    colors = fct_relevel(colors, c(random_countries, "Others"))
  ) %>%
  ggplot(aes(cancers, cardiovascular_diseases, group = country, color = colors)) +
  geom_path(aes(alpha = alpha, size = path_size)) + 
  geom_text_repel(aes(label = label_val), show.legend = F, fontface = "bold", segment.color = 'grey50') +
  scale_alpha_identity() + 
  scale_size_identity() +
  scale_color_manual(values = c("#FFBC42", "#30A9DE", "#D81159", "#218380", "#6C49B8", "#75D701", "black")) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = rel(1.8), family = "Merriweather"),
    plot.subtitle = element_text(size = rel(1.2), family = "Merriweather Light", margin = margin(0,0,20,0)),
    text = element_text(family = "Noto Sans CJK JP Light"),
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.text = element_text(size = rel(1)),
    legend.position = "top",
    legend.text = element_text(size = rel(1))
  ) +
  guides(color = guide_legend(ncol = 7, byrow = TRUE, override.aes = list(size = 3))) +
  labs(
    title = "Deaths due to Cardiovascular Diseases and Cancers\nin Randomly selected countries",
    subtitle = "Data describes causes of deaths between 1990 - 2016",
    x = "Share of Deaths due to Cancers",
    y = "Share of Deaths due to Cardiovascular Diseases",
    color = "",
    caption = "Made using Tidyverse and its extensions by Kanishka Misra\nData from Our World in Data"
  )

ggsave("week1.png", height = 11, width = 9)



