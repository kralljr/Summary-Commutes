# create Roadiness dataset

library(lubridate)
library(here)
library(DescTools)
library(tidyverse)

## ============================================
#  get roadiness gridcell data :
## ============================================
load(here("data/points_gricell.Rdata"))
load(here("data/rtypes.RData"))

# get GPS data

load(here("data/gpslatlon.RData"))
cdat0 <- filter(gpslatlon$dat, missing == 1) %>%
  rename(datetime = `Date & Time`) %>%
  mutate(datetime= as_datetime(datetime, format = "%d %B %Y %H:%M:%S", tz = "UTC"),
         rdatetime = round_date(datetime, unit = "minute"))


# bind columns:
# THESE ARE THE SAME ORDER (ORDER PULLED FROM GPSLATLON)
rtypes <- dplyr::select(rtypes, TNMFRC)
cdat0 <- bind_cols(cdat0, points_gridcell[, -c(1, 2)]) %>%
  bind_cols(., rtypes)
# remove PA
cdat0 <- filter(cdat0, !is.na(leng.distm2_scale))

tripdata <- mutate(cdat0, tripid = paste0(ID, Trip)) %>%
  arrange(tripid, datetime)
save(tripdata, file = here("data/tripdata.RData"))
# 154 trips

lev1 <- c(seq(1, 5), 8)
lab1 <- c("High/SecHigh", "High/SecHigh","LocalConn", "Local", "Other", "Other")

cdat <- cdat0 %>% mutate(rdatetime = as_datetime(rdatetime, tz = "America/New_York")) %>%
  rename(rness = leng.distm2_scale) %>%
  # remove latitude and longitude
  dplyr::select(ID, rdatetime, rness, TNMFRC) %>%
  group_by(ID, rdatetime) %>%
  summarize(rness = mean(rness), rtype = Mode(TNMFRC),
            rtype = factor(rtype, levels= lev1, labels = lab1)) %>%
  unique()


## ============================================
# get continuous PM data
## ============================================
# note: cannot merge with commute_df because R cannot distinguish between
# duplicated rows with no differentiation
load(here("data/pm-cont-data-1minrti.RData"))
pm <- dplyr::select(datall, rdatetime, rtiPM25, ID) %>%
  rename(PM = rtiPM25) %>%
  mutate(rdatetime = as_datetime(rdatetime, tz = "America/New_York")) %>%
  unique()



rcomm <- inner_join(pm, cdat)


# add information on unique commutes
rcomm <- mutate(rcomm, date = date(rdatetime)) %>%
  group_by(ID, date) %>%
  mutate(lag1 = lag(rdatetime),
         diff = as.numeric(rdatetime - lag1),
         diff = ifelse(is.na(lag1), 0, diff - 1),
         cum1 = cumsum(diff)) %>%
  ungroup() %>%
  group_by(ID, date, cum1) %>%
  mutate(obs = n()) %>%
  dplyr::select(-c(diff, lag1)) %>%
  mutate(id2 = paste(ID, date, cum1), row = row_number()) %>%
  # commutes of at least 15 minutes
  filter(obs >= 15, !is.na(PM)) %>%
  ungroup()

# save
save(rcomm, file = here("data/roadiness_commutes.Rdata"))

