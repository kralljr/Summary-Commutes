# create Roadiness dataset

library(tidyverse)
library(lubridate)
library(here)


## ============================================
#  get roadiness gridcell data :
## ============================================
load(here("data/points_gricell.Rdata"))

# get GPS data

load(here("data/gpslatlon.RData"))
cdat0 <- filter(gpslatlon$dat, missing == 1) %>%
  rename(datetime = `Date & Time`) %>%
  mutate(datetime= as_datetime(datetime, format = "%d %B %Y %H:%M:%S", tz = "UTC"),
         rdatetime = round_date(datetime, unit = "minute"))

# bind columns:
# THESE ARE THE SAME ORDER (ORDER PULLED FROM GPSLATLON)
cdat0 <- bind_cols(cdat0, points_gridcell[, -c(1, 2)])
# remove PA
cdat0 <- filter(cdat0, !is.na(leng.distm2_scale))
cdat <- cdat0 %>% mutate(rdatetime = as_datetime(rdatetime, tz = "America/New_York")) %>%
  rename(rness = leng.distm2_scale) %>%
  # remove latitude and longitude
  dplyr::select(ID, rdatetime, rness) %>%
  group_by(ID, rdatetime) %>%
  summarize(rness = mean(rness)) %>%
  unique()


## ============================================
# get continuous PM data
## ============================================
# note: cannot merge with commute_df because R cannot distinguish between
# duplicated rows with no differentiation
load(here("data/pm-cont-data.RData"))
pm <- dplyr::select(datall, rdatetime, PM, ID) %>%
  mutate(rdatetime = as_datetime(rdatetime, tz = "America/New_York")) %>%
  unique()



rcomm <- inner_join(pm, cdat)


# save
save(rcomm, file = here("data/roadiness_commutes.Rdata"))

