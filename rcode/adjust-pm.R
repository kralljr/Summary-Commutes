# adjust for PM


# load libraries
library(tidyverse)
library(here)
library(lubridate)

# load data
load(here("data/roadiness_commutes.Rdata"))
load(here("data/vah.RData"))
load(here("data/va24.RData"))


# merge with daily average for all NOVA monitors
#
rcomm <- data.frame(rcomm) %>% dplyr::filter(PM < 250) %>%
  mutate(date_local = date(rdatetime),
         # time_local not averaged
         #rounddate = round_date(rdatetime, unit = "hour"),
         rounddate = rdatetime,
         time_local = hour(rounddate), time_local = time_local + 1,
         time_local = ifelse(time_local == 24, 0, time_local)) %>%
  left_join(., va24) %>%
  left_join(., vah)

save(rcomm, file = here("data/rcomm.RData"))

