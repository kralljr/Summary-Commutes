# lat long for lucas

library(tidyverse)
library(here)
load(here("data/gpslatlon.RData"))

latlon <- gpslatlon$dat
latlon <- rowid_to_column(latlon)
latlon0 <- latlon
rownames(latlon) <- latlon$rowid
latlon <- latlon %>%
  select(Latitude, Longitude) %>%
  unique()

save(latlon, file = here("data/latlon.RData"))

latlon1 <- data.frame(latlon)
# from Lucas:
rtypes <- read.csv("data/gps_road_assigned.csv")

# diff]
# 1381 is 0,0
# latlon1 <- latlon1[-1381, ]
# rtypes1 <- rtypes[, c(1, 2)]
# rtypes1 <- data.frame(rtypes1)
# rownames(latlon1) <- seq(1, nrow(latlon1))
# rownames(rtypes1)<- seq(1, nrow(latlon1))
# all.equal(latlon1, rtypes1)
