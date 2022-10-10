# load libraries
library(rnoaa)
library(tidyverse)
library(here)
library(RColorBrewer)
library(maps)
library(forcats)

# api for NOAA: nkey
source(here("rcode/keys.R"))

load(here("data/MainStates1.RData"))

# load dates
load(here("data/gestdc-dates.RData"))

# load relative humidity
load(here("data/relativehumidity.RData"))


# rainfall: check longlat
# lonlat <- cpc_prcp(dates$date_local[1], us = T)

# get all stations
# stations <- ghcnd_stations()
# stations <- filter(stations, state %in% c("VA", "MD","DC"))
# stations <- filter(stations, !(last_year <2018 ), first_year != 2020)
# save(stations, file = here("data/stations.RData"))
load(here("data/stations.RData"))

stations1 <- filter(stations, latitude > 38.65, latitude < 39.1, longitude > -77.7, longitude < -77)
statloc <- select(stations1, id, latitude, longitude, name) %>% unique()

cols <- brewer.pal(8, "Dark2") %>% rep(10)


# g2 <- ggplot() +
#   geom_polygon( data=MainStates1, aes(x=long, y=lat, group = group),
#                 color="grey50",fill = "white") +
#   geom_point(data = statloc, aes(x = longitude, y = latitude)) +#, colour= id), alpha = 0.3) +
#   #scale_color_manual(values = cols) +
#   theme(legend.position = "none") +
#   xlab("Longitude") + ylab("Latitude")
# ggsave(g2, file = here("plots/weather.png"), width = 4, height = 4, units = "in")



# check others
elem1 <- unique(stations$element)


# elements:
# precipication, snowfall, tmax, tmin
# don't need
# https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
elrm <- c("SNWD", "WESD", "WESF", "DAPR",
          "MDPR", "TOBS")
elem1 <- unique(stations1$element)
elem1 <- elem1[-which(elem1 %in% elrm)]
# wt is specific weather: no dust measured,
# no 10: tornado
# wdf2:direction of fastest 2 minute wind
# wdf5: direction of fastest 5 minute wind
# 5 days before?



getweather <- function(station) {
  # get all variables for station
  stat1 <- ghcnd_search(
    stationid = station,
    # 3 days?
    date_min = "2018-05-05",
    date_max = "2019-03-14",
    var = "all")

  # restrict to elements of interest, with data
  stat1 <- stat1[tolower(names(stat1)) %in% tolower(elem1)]
  lens <- sapply(stat1, function(x) nrow(x))
  stat1 <- stat1[names(stat1)[lens > 0]]

  # still elements remaining
  if(length(stat1) > 0) {
  # rename/rework for tidying
  names1 <- names(stat1)
  for(i in 1 : length(stat1)) {
    # rename column
    colnames(stat1[[i]])[which(colnames(stat1[[i]]) == names1[i])] <- "value"

    # S     = Global Summary of the Day (NCDC DSI-9618)
    # NOTE: "S" values are derived from hourly synoptic reports
    # exchanged on the Global Telecommunications System (GTS).
    # Daily values derived in this fashion may differ significantly
    # from "true" daily data, particularly for precipitation
    # (i.e., use with caution).
    stat1[[i]] <- filter(stat1[[i]], sflag != "S")



    # check flags
    # 7== U.S. Cooperative Summary of the Day -- Transmitted via WxCoder3 (NCDC DSI-3207)
    # N=Community Collaborative Rain, Hail,and Snow (CoCoRaHS)
    # T = trace
    #  W     = WBAN/ASOS Summary of the Day from NCDC's Integrated  Surface Data (ISD).
    # Z     = Datzilla official additions or replacements
    temp <- filter(stat1[[i]], (mflag != " " | qflag != " " | sflag != " ") &
                     (sflag != "N") & (sflag != 7) & (sflag != "W") & (sflag != "Z"))
    if(nrow(temp) > 0) {
      # explore flags
      browser()
    }

    # add column for type
    stat1[[i]] <- mutate(stat1[[i]], type = names1[i]) %>%
      select(-c(mflag : qflag))
    if(i == 1) {
      res <- stat1[[i]]
    } else {
      res <- bind_rows(res, stat1[[i]])
    }
  }

  na.omit(res)
  }
}


# test1 <- getweather("US1DCDC0009")

# k <- 1
# for(i in 1 : nrow(statloc)) {
#   print(i)
#   stat1 <- getweather(statloc$id[i])
#   if(k == 1) {
#     weather <- stat1
#
#     if(length(stat1) > 0) {
#       k <- 2
#     }
#   } else if(length(stat1) > 0){
#     weather <- full_join(weather, stat1)
#   }
# }
# save(weather, file = here("data/weather-raw.RData"))

load(here("data/weather-raw.RData"))

# check variation in wind direction
load(here("data/gestdc-dates.RData"))

select(weather, -sflag) %>%
  filter(type == "wdf5") %>%
  group_by(date) %>%
  summarize(sd = sd(value, na.rm = T), mean = mean(value, na.rm = T),
            min = min(value, na.rm = T), max = max(value, na.rm = T)) %>%
  ungroup() %>%
  filter(sd > 0) %>%
  select(-sd) %>%
  pivot_longer(mean : max) %>%
  mutate(value = case_when((value <= 11 | value >=349) ~ "N",
                   (value >= 12 & value <= 33) ~ "NNE",
                   (value >= 34 & value <= 56) ~ "NE",
                   # N = North (349 - 011 degrees)
                   # NNE = North-Northeast (012-033 degrees)
                   # NE = Northeast (034-056 degrees)

                   (value  >= 57 & value <= 78) ~ "ENE",
                   (value >= 79 & value  <= 101) ~ "E",
                   (value >= 102 & value <= 123) ~ "ESE",
                   (value >= 124 & value <= 146) ~ "SE",
                   (value >= 147 & value <= 168) ~ "SSE",
                   (value >= 169 & value <= 191) ~ "S",
                   # ENE = East-Northeast (057-078 degrees)
                   # E = East (079-101 degrees)
                   # ESE = East-Southeast (102-123 degrees)
                   # SE = Southeast (124-146 degrees)
                   # SSE = South-Southeast (147-168 degrees)
                   # S = South (169-191 degrees)

                   (value >= 192 & value <= 213) ~ "SSW",
                   (value >= 214 & value <= 236) ~ "SW",
                   (value >= 237 & value <= 258) ~ "WSW",
                   (value >= 259 & value <= 281) ~ "W",
                   (value >= 282 & value <= 303) ~ "WNW",
                   # SSW = South-Southwest (192-213 degrees)
                   # SW = Southwest (214-236 degrees)
                   # WSW = West-Southwest (237-258 degrees)
                   # W = West (259-281 degrees)
                   # WNW = West-Northwest (282-303 degrees)
                   (value >= 304 & value <= 326) ~ "NW",
                   (value >= 327 & value <= 348) ~ "NNW"),
         value = fct_collapse(value, NW = c("NW", "NNW", "W", "WNW", "N"),
                               SE =c("S", "SE", "ESE", "SSE", "SSE", "E")),
         # other: NNE, SSW, WSW
         value = fct_other(value, keep = c("NW", "SE"))) %>%
  filter(name != "mean") %>%
  pivot_wider() %>% filter(min != max) %>%
  rename(date_local = date) %>%
  inner_join(dates, .) %>% nrow()

# discrepancies
# 19 / 79: 25% misclassification?


weather <- select(weather, -sflag) %>%
  group_by(date, type) %>%
  summarize(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = "type") %>%
  # thunder
  mutate(icehailgl = ifelse(wt04 == 1 | wt05 == 1 | wt06 == 1, 1, NA),
         fog = ifelse(wt01 == 1 | wt02 == 1, 1, NA),
         # drifting/blowing snow?
         windsnow = ifelse(wt11 ==1 | wt09 == 1, 1, NA),

         # check date lag
         #dateL1 = dplyr::lag(date),
         #diff = as.numeric(date - dateL1),
         tmaxL1 = dplyr::lag(tmax), tminL1 = dplyr::lag(tmin),
         awndL1 = dplyr::lag(awnd), prcpL1 = dplyr::lag(prcp),
         snowL1 = dplyr::lag(snow),
         tmaxL1m = dplyr::lead(tmax), tminL1m = dplyr::lead(tmin),
         awndL1m = dplyr::lead(awnd), prcpL1m = dplyr::lead(prcp),
         snowL1m = dplyr::lead(snow)) %>%
  rename(smoke = wt08, date_local = date) %>%
  select(-c(wt03, wt04, wt05, wt01, wt09, wt11, wt06, wt02,
            # remove fastest wind speed?
            wsf2, wsf5))

select(weather, date, prcp, prcpL1) %>% View()

# merge RH
weather0 <- weather
# only RH for days of GESTDC data- fine
rh <- dplyr::select(rh, -gestdc)
weather <- full_join(weather, rh)

# wind direction in degrees
save(weather, file = here("data/weather-cleaned.RData"))




#
# # Try to get humidity
# # no humidity for these sites
# stat0 <- read.table("~/Dropbox/CommuteVar/data/ghcnd-stations-edit.txt", sep = "\t",
#                     header = F, na = " ")
# stat1 <- mutate(stat0, id = substr(V1, 1, 11),
#                 lat = substr(V1, 13, 20),
#                 long = substr(V1, 23, 30),
#                 lat = as.numeric(lat),
#                 long = as.numeric(long)) %>%
#   filter(lat > 38.65, lat < 39.1, long > -77.7, long < -77)
# nrow(stat1)
# k <- 1
# for(i in 1 : nrow(stat1)) {
#   dat0 <- ghcnd(stationid = stat1$id[i])
#   dat0 <- dplyr::filter(dat0, element %in% c("ADPT", "RHAV", "RHMN", "RHMX"))
#   if(nrow(dat0)> 0) {
#     if(k == 1) {
#       datall <- dat0
#       k <- k + 1
#     } else {
#       datall <- bind_rows(datall, dat0)
#     }
#   }
# }
#
#
