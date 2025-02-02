# adjust for PM


# load libraries
library(tidyverse)
library(here)
library(lubridate)

# load data
load(here("data/roadiness_commutes.Rdata"))
load(here("data/vah.RData"))
load(here("data/va24.RData"))
load(here("data/weather-cleaned.RData"))

rcomm0 <- rcomm
# merge with daily average for all NOVA monitors
#
rcomm <- data.frame(rcomm0) %>%
  # already removed? in roadiness_dataset.R
  # dplyr::filter(PM < 250) %>%
  mutate(date_local = date(rdatetime),
         # time_local not averaged
         #rounddate = round_date(rdatetime, unit = "hour"),
         rounddate = rdatetime,
         # EPA time is start of monitoring period
         time_local = hour(rounddate),
         # assumes hourly monitors capture preceding hour
         #time_local = time_local + 1,
         time_local = ifelse(time_local == 24, 0, time_local),
         month = month(rdatetime)) %>%
  left_join(., va24) %>%
  left_join(., vah) %>%
  left_join(., vah0) %>%
  left_join(., weather) %>%
  group_by(ID, date_local) %>%
  mutate(lag2 = lag(rdatetime), diff = as.numeric(rdatetime - lag2),
         diff1 = ifelse(diff > 1, 1, 0),
         diff1 = ifelse(is.na(diff1), 0, diff1),
         group = cumsum(diff1),
         id2 = paste0(ID, date_local, group),
         id3 = paste(date_local, group)) %>%
  dplyr::select(-c(diff, diff1, lag2)) %>%
  ungroup()




save(rcomm, file = here("data/rcomm.RData"))

