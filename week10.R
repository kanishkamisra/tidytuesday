library(tidyverse)
library(lubridate)
library(ggbeeswarm)

bikeshare <- dir(path = "data/PublicTripData/", pattern = "*.csv") %>%
  map(~read.csv(file.path("data/PublicTripData", .))) %>%
  reduce(bind_rows) %>%
  as_tibble()

bikeshare %>%
  mutate(
   duration = as.numeric(as.duration(hms(Duration)), "minutes")
 ) %>%
  # select(duration, Duration) %>%
  filter(duration < 10000, Distance_Miles < 1000) %>%
  ggplot(aes(PaymentPlan, duration)) + 
  geom_quasirandom()

bikeshare %>% 
  filter(Duration == "") %>%
  select(RouteID)

bikeshare %>%
  select(StartDate, StartTime, EndDate, EndTime, TripType) %>%
  mutate(
    duration = as.numeric(as.duration(mdy_hm(str_c(StartDate, StartTime)) %--% mdy_hm(str_c(EndDate, EndTime))), "minutes")
  ) %>%
  filter(duration > 10000)






