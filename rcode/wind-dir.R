# fix wind
# load libraries
library(tidyverse)
library(here)


load(here("data/rcomm.Rdata"))


# https://www7.ncdc.noaa.gov/climvis/help_wind.html

rcomm <- mutate(rcomm, cat2 = case_when((wdf2 <= 11 | wdf2 >=349) ~ "N",
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
                cat5sm = fct_other(cat5sm, keep = c("NW", "SE")))

save(rcomm, file = here("data/rcomm.RData"))
