# file to obtain all the GPS lat/lon data

library(tidyverse)
library(here)
library(data.table)

source(here("functions/GMU_commute.R"))

# ignore warning (final line just contains id)  "gps-only/104701.csv"
# Warning in data.table::fread(file, skip = 3, select = c(2:18)) :
  # Discarded single-line footer: <<73057 >>
gpslatlon <- oGMU_commute("gps-only/")
save(gpslatlon, file = here("data/gpslatlon.RData"))


# check files: 36 with GPS files
# 28 with non-missing latitude and longitude
list.files("~/Documents/gestdc-data/Data-logger/gps-only/") %>%
  substr(., 1, 4) %>% unique() %>% length()
