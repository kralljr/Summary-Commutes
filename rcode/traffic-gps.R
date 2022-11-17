# file to create speed data

library(tidyverse)
library(here)
library(lubridate)

# data
load(here("data/gpslatlon.RData"))
dat0 <- gpslatlon$dat



# dist function
distfun <- function(Longitude, laglon, Latitude, laglat) {
  # sqrt((Longitude - laglon)^2 + (Latitude - laglat)^2)


  Longitude <- Longitude * pi / 180
  Latitude <- Latitude * pi / 180
  laglon <- laglon * pi / 180
  laglat <- laglat * pi / 180



  absdiff <- abs(Longitude - laglon)
  diffsig <- acos(sin(Latitude) * sin(laglat) + cos(Latitude) * cos(laglat)* cos(absdiff))
  r <- 6371.009

  # num <- sqrt((cos(laglat) * sin(absdiff))^2 +
  #               (cos(Latitude) * sin(laglat) -sin(Latitude) * cos(laglat) * cos(absdiff))^2)
  # den <- sin(Latitude) * sin(laglat) + cos(Latitude) * cos(laglat) * cos(absdiff)
  # diffsig <- atan(num / den)

  # from km to miles
  dist <- r * diffsig * 0.621371
  sames <- (Latitude == laglat) & (Longitude ==  laglon)
  dist[sames] <- 0
  dist
}




# Distances
# get lag
dat <- dat0 %>%
  # remove non-0 lat/lon
  filter(missing == 1) %>%
  dplyr::select(ID, `Date & Time`, Latitude, Longitude, Trip) %>%
  mutate( datetime = dmy_hms(`Date & Time`), datetime2 = with_tz(datetime, "America/New_York"),
          day = date(datetime2)) %>%
  group_by(ID, Trip, day) %>%
  # get distance from next point
  mutate(laglon = lag(Longitude), laglat = lag(Latitude)) %>%
  dplyr::filter(!is.na(laglon) & !is.na(laglat)) %>%
  mutate(dist = distfun(Longitude, laglon, Latitude, laglat))



# compute MPH by trip/day (no)
#
# dat1 <- dat %>% mutate(rdatetime = round(datetime, "mins"))%>%
#   ungroup() %>%
#   dplyr::select(c(ID, rdatetime, Trip, dist, day)) %>%
#   # find distance for each minute
#   group_by(ID, Trip, day) %>%
#   summarize(sdist = sum(dist), mint = min(rdatetime), maxt = max(rdatetime),
#             diff1 = maxt - mint,
#             diff0 = difftime(maxt, mint, units = "hours"),
#             diff = as.numeric(diff1)/60 / 60) %>%
#   mutate(mph = sdist / diff) %>%
#   # remove time 0 commutes
#   dplyr::filter(diff != 0)
#
# # chekc large
# filter(dat1, mph > 100)





# Compute MPH by minute (yes)

# Running mean speed
speed <- dat %>% mutate(rounddate = round(datetime, "mins"))%>%
  ungroup() %>%
  dplyr::select(c(ID, rounddate, Trip, dist, day)) %>%
  # mutate(mph = dist * 60) %>%
  # find distance for each minute
  group_by(ID, rounddate, day) %>%
  summarize(sdist = sum(dist)) %>%
  mutate(mph = sdist * 60, mph = ifelse(mph > 100, NA, mph))  %>%
  dplyr::select(-c(day, sdist)) %>% unique()

summary(speed$mph)
sort(speed$mph, decreasing = T)[1 : 20]
# boxplot(speed$mph)
# speed0 <- filter(speed, mph > 90) %>% dplyr::select(ID, rdatetime, day) %>% unique()
# speed1 <- left_join(speed0, speed)

# ggplot(speed1, aes(x = rdatetime, y= mph)) +
#   geom_line() +
#   facet_wrap(ID ~ Trip + day, ncol = 3, scales = "free")





save(speed, file = here("data/speed.RData"))

