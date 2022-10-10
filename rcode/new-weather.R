# new weather data

library(tidyverse)

# load dates
load(here("data/gestdc-dates.RData"))

# load/format rh
weat <- read.csv("~/Dropbox/CommuteVar/data/3103782.csv", na.strings = (""))
cn <- colnames(weat)
keeps <- c("STATION", "DATE", "BackupLatitude", "BackupLongitude",
            "HourlyDewPointTemperature", "HourlyPrecipitation", "HourlyRelativeHumidity",
"HourlyWindSpeed", "HourlyWindDirection", "HourlyWetBulbTemperature",
"HourlyDryBulbTemperature") # use dry bulb for air temp, RH better than DP
weat <- weat[, keeps]
rh <- mutate(weat, date_local = date(DATE)) %>%
  dplyr::select(date_local, HourlyRelativeHumidity) %>%
  group_by(date_local) %>%
  summarize(RH = mean(HourlyRelativeHumidity, na.rm = T)) %>%
  left_join(dates, .)

save(rh, file = here("data/relativehumidity.RData"))
