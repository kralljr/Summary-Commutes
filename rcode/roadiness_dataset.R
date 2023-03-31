# create Roadiness dataset

library(lubridate)
library(here)
library(DescTools)
library(tidyverse)

## ============================================
#  get roadiness gridcell data :
## ============================================
load(here("data/points_gricell-new.Rdata"))
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
cdat0 <- filter(cdat0, !is.na(leng.distm2_loc_scale))

tripdata <- mutate(cdat0, tripid = paste0(ID, Trip)) %>%
  arrange(tripid, datetime)



# check sample size
# 36
dplyr::select(gpslatlon$dat, ID) %>% unique() %>% nrow()
# 28: missing = 1
dplyr::select(cdat0, ID) %>% unique() %>% nrow()



save(tripdata, file = here("data/tripdata.RData"))
# 154 trips

# TNMFRC contains 5 and 8s only for other

lev1 <- c(seq(1, 5), 8)
lab1 <- c("High/SecHigh", "High/SecHigh","LocalConn", "Local", "Other", "Other")

cdat <- cdat0 %>% mutate(rdatetime = as_datetime(rdatetime, tz = "America/New_York")) %>%
  rename(rness1loc = leng.distm2_loc_scale,
         rness500loc = leng.distm2_loc_scale500,
         rness1hw = leng.distm2_hw_scale,
         rness500hw = leng.distm2_hw_scale500) %>%
  # remove latitude and longitude
  # TNMFRC contains 5 and 8s only for other
  dplyr::select(ID, rdatetime, rness1loc,
                rness500loc, rness1hw, rness500hw, TNMFRC) %>%
  group_by(ID, rdatetime) %>%
  summarize(rness1loc = mean(rness1loc),
            rness500loc = mean(rness500loc),
            rness1hw = mean(rness1hw),
            rness500hw = mean(rness500hw),
            rtype = Mode(TNMFRC),
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

# check code below-- should be the same
pm1b <- mutate(pm, date = date(rdatetime))
pm1 <- pm1b %>%
  filter(!is.na(PM), PM < 250) %>%
  group_by(ID, date) %>%
  arrange(ID, date, rdatetime) %>%
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


save(pm1, pm1b, file =  here("data/pm-cleaned.RData"))

#49
# unique(pm$ID) %>% length()
# 28
# unique(cdat$ID) %>% length()
rcomm <- inner_join(pm, cdat)
# 27
# unique(rcomm$ID) %>% length()

# add information on unique commutes
rcomm <- mutate(rcomm, date = date(rdatetime)) %>%
  filter(!is.na(PM), PM < 250) %>%
  group_by(ID, date) %>%
  arrange(ID, date, rdatetime) %>%
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

