# file to find unique locations
library(here)
library(tidyverse)

# get all data
load(here("data/gpslatlon.RData"))
dat <- gpslatlon$dat

# filter to yes, lat/lon only
unique_commutes_df <- dplyr::select(dat, Latitude, Longitude, missing) %>%
  filter(missing == 1) %>%
  unique()

save(unique_commutes_df, file = here("data/unique_commutes_df.RData"))
