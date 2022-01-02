# load libraries
library(tidyverse)
library(here)
library(readxl)
library(lubridate)

# load data
load(here("data/va_monitor_data.RData"))

# site 0030 has 1 hour and 24 hour
va <- filter(va_monitor_data, county_code %in% c("013", "059")) %>%
  mutate(year = substr(date_local, 1, 4), year = as.numeric(year)) %>%
  filter(year %in% c(2018, 2019)) %>%
  select(AQS_site_ID, latitude, longitude, date_local, time_local, sample_measurement,
         sample_duration)





#################################
# dates for GEST-DC
dates <- read_excel("~/Dropbox/GESTDC/data/metals-metadata/GMU_RTI_Metadata-loq-23oct19.xlsx") %>%
  dplyr::filter(substr(FilterID, 1, 2) == "GM", FilterID != "GMU1025") %>%
  dplyr::select(strt_date, stp_date) %>%
  dplyr::mutate(stp_date= ifelse(stp_date == as_datetime("2018-01-18"), as_datetime("2019-01-18"), stp_date),
                stp_date = as_datetime(stp_date),
                mid = as.numeric(stp_date - strt_date),
         stp_date = date(stp_date),
         strt_date = date(strt_date),
         mid = ifelse(mid == 2, strt_date + days(1), strt_date),
         mid = as.Date(mid, origin = "1970-01-01")) %>%
  rowid_to_column() %>%
  pivot_longer(-rowid) %>%
  dplyr::select(value) %>% unique() %>%
  mutate(gestdc = 1)%>% rename(date_local = "value")

save(dates, file = here("data/gestdc-dates.RData"))


######################
### Add in missing dates: impute with average of day before, day after
datem <- filter(dates, date_local %in% as.Date(c("2019-02-06", "2019-03-05")))

vam <- filter(va, date_local %in% (c("2019-02-05", "2019-02-06",
                                     "2019-02-07", "2019-03-04", "2019-03-05", "2019-03-06"))) %>%
  filter(sample_duration == "24 HOUR") %>%
  arrange(date_local) %>%
  na.omit() %>%
  mutate(date_local = ifelse(date_local %in% c("2019-02-05", "2019-02-07"), "2019-02-06",
                             "2019-03-05"))

va <- full_join(va, vam)





############################################
# 24 hour monitors: detrend and obtain daily average for dates needed
va24 <- filter(va, sample_duration == "24 HOUR")

# detrend
va24 <- group_by(va24, AQS_site_ID) %>%
  mutate(mean = mean(sample_measurement, na.rm = T),
         diff = sample_measurement - mean,
         date_local = as.Date(date_local)) %>%
  select(AQS_site_ID, date_local, mean, diff,
         sample_measurement) %>%
  ungroup()

# overall mean
omean <- select(va24, AQS_site_ID, mean) %>%
  unique() %>% with(., mean(mean))

# average by day, add in overall mean
va24a <- group_by(va24, date_local) %>%
  summarize(Detrend = mean(diff, na.rm = T) + omean,
            Mean = mean(sample_measurement, na.rm = T)) %>%
  pivot_longer(Detrend : Mean, values_to = "sample_measurement",
               names_to = "AQS_site_ID")

va24 <- full_join(va24, va24a)

# all correlated > 0.95
# select(va24, -c(mean, diff)) %>%
#   pivot_wider(., names_from = "AQS_site_ID",
#             values_from = "sample_measurement") %>%
#   select(-c(date_local)) %>%
#   cor(use = "pair")

# all observed for time period: looks similar
ggplot(va24, aes(x = date_local, y = sample_measurement,
                 colour = AQS_site_ID)) +
  geom_line(alpha = 0.7)
ggsave(here("timeseries.png"), width = 10, height = 5, units = "in")


va24 <- filter(va24, AQS_site_ID == "Mean") %>%
  select(date_local, sample_measurement) %>%
  rename(daily = sample_measurement)
va24a <- va24

# merge with dates
va24 <- left_join(dates, va24) %>%
  select(-gestdc)

save(va24, file = here("data/va24.RData"))





############################################
# Hourly concentrations (for hourly effects)
# only one monitor (springfield)
vah0 <- filter(va, sample_duration != "24 HOUR") %>%
  select(date_local, time_local, sample_measurement) %>%
  rename(hourly = sample_measurement) %>%
  mutate(month = substr(date_local, 6, 7), month = as.numeric(month),
         date_local = as.Date(date_local), time_local = as.numeric(substr(time_local, 1, 2)))%>%
  #left_join(., va24a) %>%
  na.omit() %>%
  group_by(date_local) %>%
  # daily mean from hourly monitors
  mutate(mean = mean(hourly)) %>% ungroup() %>%
  # subtract off regional effect
  mutate(diff = hourly - mean) %>%
  group_by(time_local, month) %>%
  mutate(meanS = mean(diff)) %>%
  ungroup() %>%
  group_by(time_local) %>%
  mutate(mean = mean(diff))

vah <- vah0

# not large temporal/hourly effects
ggplot(vah) +
  geom_line(aes(x = time_local, y = diff, group = date_local), alpha = 0.1) +
  geom_line( aes(x = time_local, y = mean), colour = "red")

# by month
ggplot(vah) +
  geom_line(aes(x = time_local, y = diff, group = date_local), alpha = 0.1) +
  geom_line( aes(x = time_local, y = meanS), colour = "red") +
  facet_wrap(~month)

# by month
vah2 <- mutate(vah, month = as.numeric(month))
ggplot(vah2) +
  geom_line( aes(x = time_local, y = meanS, colour = month, group = month)) +
  geom_line(aes(x = time_local, y = mean), colour = "red")


vah <- select(vah, time_local, month, mean, meanS) %>%
  rename(hourly = mean, hourlymonth = meanS) %>%
  unique()


# availability on GEST DC dates?
load(here("data/gestdc-dates.RData"))
# all dates available
join1 <- left_join(dates, vah0)

vah0 <- rename(vah0, obshour = hourly)



save(vah, vah0, file = here("data/vah.RData"))


# only started in 2020
# vah1 <- filter(va, site_number == "0030", county_code == "059",
#    sample_duration == "1 HOUR")
