# file to obtain all the GPS lat/lon data

library(tidyverse)
library(here)
library(data.table)

source("GMU_commute.R")

# ignore warning (final line just contains id)  "gps-only/104701.csv"
# Warning in data.table::fread(file, skip = 3, select = c(2:18)) :
  # Discarded single-line footer: <<73057 >>
# gpslatlon <- oGMU_commute("gps-only/")
# save(gpslatlon, file = here("data/gpslatlon.RData"))

# load(here("data/gpslatlon.RData"))

dat <- gpslatlon$dat

select(dat, ID, Trip) %>% unique() %>% count(ID)


trips <- gpslatlon$trips

# total summary per participant
filter(trips, Trip == "Overall") %>%
  filter(sum > 0)

# trip summaries
tripsObs <- filter(trips, Trip != "Overall") %>%
  group_by(ID) %>%
  rename(propobs = mean) %>%
  # summary of avail data per participant
  summarize(mean = mean(propobs), min = min(propobs), max = max(propobs)) %>%
  filter(max > 0)

dim(trips)
# 28 participants with some
arrange(tripsObs, max)
arrange(tripsObs, mean) %>% View()


untime <- select(dat, ID, `Date & Time`, missing) %>%
  filter(missing == 1) %>%
  unique()

untime %>% dim()
# 197160 seconds, #3286 minutes
197160 / 60 / 60 # = 54 hours

