library(tidyverse)
library(lubridate)

superheroes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week9_comic_characters.csv")

superheroes %>%
  count(year, eye) %>%
  # arrange(-n) %>%
  filter(!is.na(eye)) %>%
  ggplot(aes(year, n, color = eye, group = eye)) +
  geom_line()
