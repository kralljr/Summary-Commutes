# full data no GPS

# load libraries
library(here)
library(tidyverse)
library(lubridate)


# load data
load(here("data/pm-cleaned.RData"))
load(here("data/vah.RData"))
load(here("data/va24.RData"))
load(here("data/weather-cleaned.RData"))


# when commuting from GEST DC paper
load("/Users/jenna/Dropbox/GESTDC/data/commute-data.RData")


#
pmf <- data.frame(pm1) %>% dplyr::filter(PM < 250) %>%
  mutate(date_local = date(rdatetime),
         # time_local not averaged
         #rounddate = round_date(rdatetime, unit = "hour"),
         rounddate = rdatetime,
         # EPA time is start of monitoring period
         time_local = hour(rounddate),
         # assumes hourly monitors capture preceding hour
         #time_local = time_local + 1,
         time_local = ifelse(time_local == 24, 0, time_local),
         month = month(rdatetime)) %>%
  left_join(., va24) %>%
  left_join(., vah) %>%
  left_join(., vah0) %>%
  left_join(., weather) %>%
  group_by(ID, date_local) %>%
  mutate(lag2 = lag(rdatetime), diff = as.numeric(rdatetime - lag2),
         diff1 = ifelse(diff > 1, 1, 0),
         diff1 = ifelse(is.na(diff1), 0, diff1),
         group = cumsum(diff1),
         id2 = paste0(ID, date_local, group),
         id3 = paste(date_local, group)) %>%
  dplyr::select(-c(diff, diff1, lag2)) %>%
  ungroup()



# convert from tenths
t10 <- function(t) t / 10
binfun <- function(x) ifelse(x > 0, 1, 0)

pmf <- mutate(pmf,

                 #### WIND SPEED


                 cat2 = case_when((wdf2 <= 11 | wdf2 >=349) ~ "N",
                                  (wdf2 >= 12 & wdf2 <= 33) ~ "NNE",
                                  (wdf2 >= 34 & wdf2 <= 56) ~ "NE",
                                  # N = North (349 - 011 degrees)
                                  # NNE = North-Northeast (012-033 degrees)
                                  # NE = Northeast (034-056 degrees)

                                  (wdf2  >= 57 & wdf2 <= 78) ~ "ENE",
                                  (wdf2 >= 79 & wdf2  <= 101) ~ "E",
                                  (wdf2 >= 102 & wdf2 <= 123) ~ "ESE",
                                  (wdf2 >= 124 & wdf2 <= 146) ~ "SE",
                                  (wdf2 >= 147 & wdf2 <= 168) ~ "SSE",
                                  (wdf2 >= 169 & wdf2 <= 191) ~ "S",
                                  # ENE = East-Northeast (057-078 degrees)
                                  # E = East (079-101 degrees)
                                  # ESE = East-Southeast (102-123 degrees)
                                  # SE = Southeast (124-146 degrees)
                                  # SSE = South-Southeast (147-168 degrees)
                                  # S = South (169-191 degrees)

                                  (wdf2 >= 192 & wdf2 <= 213) ~ "SSW",
                                  (wdf2 >= 214 & wdf2 <= 236) ~ "SW",
                                  (wdf2 >= 237 & wdf2 <= 258) ~ "WSW",
                                  (wdf2 >= 259 & wdf2 <= 281) ~ "W",
                                  (wdf2 >= 282 & wdf2 <= 303) ~ "WNW",
                                  # SSW = South-Southwest (192-213 degrees)
                                  # SW = Southwest (214-236 degrees)
                                  # WSW = West-Southwest (237-258 degrees)
                                  # W = West (259-281 degrees)
                                  # WNW = West-Northwest (282-303 degrees)
                                  (wdf2 >= 304 & wdf2 <= 326) ~ "NW",
                                  (wdf2 >= 327 & wdf2 <= 348) ~ "NNW"),
                 # NW = Northwest (304-326 degrees)
                 # NNW = North-Northwest (327-348 degrees)
                 # VAR = Variable wind direction
                 # CLM = Calm winds (speed = 0 knots)
                 cat5 = case_when((wdf5 <= 11 | wdf5 >=349) ~ "N",
                                  (wdf5 >= 12 & wdf5 <= 33) ~ "NNE",
                                  (wdf5 >= 34 & wdf5 <= 56) ~ "NE",
                                  # N = North (349 - 011 degrees)
                                  # NNE = North-Northeast (012-033 degrees)
                                  # NE = Northeast (034-056 degrees)

                                  (wdf5  >= 57 & wdf5 <= 78) ~ "ENE",
                                  (wdf5 >= 79 & wdf5  <= 101) ~ "E",
                                  (wdf5 >= 102 & wdf5 <= 123) ~ "ESE",
                                  (wdf5 >= 124 & wdf5 <= 146) ~ "SE",
                                  (wdf5 >= 147 & wdf5 <= 168) ~ "SSE",
                                  (wdf5 >= 169 & wdf5 <= 191) ~ "S",
                                  # ENE = East-Northeast (057-078 degrees)
                                  # E = East (079-101 degrees)
                                  # ESE = East-Southeast (102-123 degrees)
                                  # SE = Southeast (124-146 degrees)
                                  # SSE = South-Southeast (147-168 degrees)
                                  # S = South (169-191 degrees)

                                  (wdf5 >= 192 & wdf5 <= 213) ~ "SSW",
                                  (wdf5 >= 214 & wdf5 <= 236) ~ "SW",
                                  (wdf5 >= 237 & wdf5 <= 258) ~ "WSW",
                                  (wdf5 >= 259 & wdf5 <= 281) ~ "W",
                                  (wdf5 >= 282 & wdf5 <= 303) ~ "WNW",
                                  # SSW = South-Southwest (192-213 degrees)
                                  # SW = Southwest (214-236 degrees)
                                  # WSW = West-Southwest (237-258 degrees)
                                  # W = West (259-281 degrees)
                                  # WNW = West-Northwest (282-303 degrees)
                                  (wdf5 >= 304 & wdf5 <= 326) ~ "NW",
                                  (wdf5 >= 327 & wdf5 <= 348) ~ "NNW"),
                 cat2sm = fct_collapse(cat2, NW = c("NW", "NNW", "W", "WNW", "N"),
                                       SE =c("S", "SE", "ESE", "SSE", "SSE", "E")),
                 # other: NNE, SSW, WSW
                 cat2sm = fct_other(cat2sm, keep = c("NW", "SE")),
                 cat5sm = fct_collapse(cat5, NW = c("NW", "NNW", "W", "WNW", "N"),
                                       SE =c("S", "SE", "ESE", "SSE", "SSE", "E")),
                 # other: NNE, SSW, WSW
                 cat5sm = fct_other(cat5sm, keep = c("NW", "SE")),



                 #### LOG PM
                 lPM = log(PM + 0.01),

                 ### Units of weather
                 # originally tenths m/s or tenths of degC
                 tmax = t10(tmax), tmaxL1 = t10(tmaxL1), tmaxL1m = t10(tmaxL1m),
                 tmin = t10(tmin), tminL1 = t10(tminL1), tminL1m = t10(tminL1m),
                 awnd = t10(awnd), awndL1 = t10(awndL1), awndL1m = t10(awndL1m),


                 # snow and prcp binary
                 snowbin = binfun(snow),
                 snowbinL1 = binfun(snowL1),
                 snowbinL1m = binfun(snowL1m),
                 prcpbin = binfun(prcp),
                 prcpbinL1 = binfun(prcpL1),
                 prcpbinL1m = binfun(prcpL1m))


# mode of road type
pmf <- group_by(pmf, ID, date_local, group) %>%
  mutate(stime = min(rdatetime),
         timemin = as.numeric((rdatetime - stime)/60) ) %>% ungroup() %>%
  select(-stime)


seqs <- function(dat) {
  dat <- mutate(dat, start = int_start(int1), end = int_end(int1))
  rdatetime <- seq(dat$start, dat$end, by = "min")
  data.frame(rdatetime)
}

## keep only commutes (>=15 minutes)
xall1 <- filter(xall, diffn >= 15) %>% select(id, int) %>%
  mutate(int1 = int) %>% nest(data = c(int1)) %>%
  # find all times corresponding to commute
  mutate(seqs = purrr::map(data, ~ seqs(dat = .))) %>%
  select(-data) %>%
  unnest(seqs) %>%
  select(-int) %>%
  rename(ID = id)


# restrict to commute times
pmf <- left_join(xall1, pmf, by = c("ID", "rdatetime"))

save(pmf, file = here("data/pm-final.RData"))

