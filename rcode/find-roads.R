# lat long for lucas

library(tidyverse)
library(here)
load(here("data/gpslatlon.RData"))

latlon <- gpslatlon$dat
latlon <- rowid_to_column(latlon)
latlon0 <- latlon
rownames(latlon) <- latlon$rowid
latlon <- latlon %>%
  select(Latitude, Longitude, missing) %>%
  unique() %>%
  # row 1381 was missing lat/lon# 1381 is 0,0
  filter(missing == 1) %>% select(-missing)

# All group IDS
glatlon <- latlon0 %>%
  group_by(Latitude, Longitude) %>%
  filter(missing == 1)  %>%
  mutate(group = cur_group_id(), diff = rowid  -group) %>%
  ungroup()
glatlonU <- select(glatlon, Latitude, Longitude, group) %>% unique()
groupIDs <- glatlonU[, 3]

# check that glatlon/latlon are same except unique
# glatlonU1 <- glatlonU[, c(1, 2)]
# glatlonU1 <- data.frame(glatlonU1)
# latlon1 <- data.frame(latlon)
# rownames(latlon1) <- seq(1, nrow(latlon1))
# rownames(glatlonU1)<- seq(1, nrow(glatlonU1))
# all.equal(latlon1, glatlonU1)

save(latlon, file = here("data/latlon.RData"))

latlon1 <- data.frame(latlon)
# from Lucas:
rtypes <- read.csv(here("data/gps_road_assigned.csv"))
rtypes <- mutate(rtypes, group = groupIDs$group) %>%
  select(group, TNMFRC) %>%
  full_join(glatlon, .) %>%
  filter(missing == 1)

# same order as cdat0

save(rtypes, file = here("data/rtypes.RData"))


# check
# load(here("data/gpslatlon.RData"))
# cdat0 <- filter(gpslatlon$dat, missing == 1)
# cdat0 <- select(cdat0, Latitude, Longitude)
# rtypes1 <- select(rtypes, Latitude, Longitude)
# rtypes1 <- data.frame(rtypes1)
# cdat0 <- data.frame(cdat0)
# rownames(rtypes1) <- seq(1, nrow(rtypes1))
# rownames(cdat0)<- seq(1, nrow(rtypes1))
# all.equal(rtypes1, cdat0)

# diff
# rtypes1 <- rtypes[, c(1, 2)]
# rtypes1 <- data.frame(rtypes1)
# rownames(latlon1) <- seq(1, nrow(latlon1))
# rownames(rtypes1)<- seq(1, nrow(latlon1))
# all.equal(latlon1, rtypes1)
