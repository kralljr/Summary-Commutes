# create Roadiness dataset

library(tidyverse)
library(lubridate)

## ============================================
# get continuous PM data
## ============================================
# note: cannot merge with commute_df because R cannot distinguish between
# duplicated rows with no differentiation
load(here("data/pm-cont-data.RData"))
pm <- select(datall, rdatetime, PM, ID) %>%
  mutate(rdatetime = as_datetime(rdatetime, tz = "America/New_York")) %>%
  unique()

load(here("data/gpslatlon.RData"))
cdat0 <- filter(gpslatlon$dat, missing == 1) %>%
  rename(datetime = `Date & Time`) %>%
  mutate(datetime= as_datetime(datetime, format = "%d %B %Y %H:%M:%S", tz = "UTC"),
         rdatetime = round_date(datetime, unit = "minute"))

cdat <- cdat0 %>% mutate(rdatetime = as_datetime(rdatetime, tz = "America/New_York")) %>%
  select(-missing) %>%
  unique()

pmdat <- inner_join(pm, cdat)

## ============================================
#  get roadiness gridcell data :
## ============================================
load(here("data/points_gricell.Rdata"))
points_gridcell <- unique(points_gridcell)

## ============================================
# create roadiness dataset :
## ============================================

# inner join commutes and roadiness data :
rpm <- left_join(pmdat, points_gridcell)
# Remove missing: these are the commutes in eastern MD, PA
roadiness_commutes <- na.omit(roadiness_commutes)

# reorder columns
roadiness_commutes <- roadiness_commutes[, c("Date & Time", "participant", "Longitude", "Latitude", "leng.distm2_scale", "grid_cell")]


# save
save(roadiness_commutes, file = "roadiness_commutes.Rdata")

