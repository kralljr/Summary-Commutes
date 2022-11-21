# plot commute locations

library(tidyverse)
library(maps)
library(here)
library(RColorBrewer)

# load data
load(here("data/rcomm2.RData"))

# load GPS
load(here("data/tripdata.RData"))

tripdata1 <- tripdata %>%
  # remove PA

 # filter(., ID != "GMU1026") %>%
  mutate(hr = as.numeric(substr(trip_total_time, 1, 2)),
         min = as.numeric(substr(trip_total_time, 4, 5)),
         timetotal = hr * 60 + min) %>%
  # need to merge wqith PM
  dplyr::filter(timetotal >=15)
loc <- tripdata1[, c("ID", "Longitude", "Latitude")] %>% as.data.frame()

# plot(loc[, "Longitude"], loc[, "Latitude"])

# -78, -76
# 38.5, 39.4
# load map data
MainStates <- map_data("county")

# "culpeper","spotsylvania""fauquier","clarke",
subs <- c("arlington",  "fairfax",
          "loudoun", "prince william" )
subs1 <- c("prince georges", "montgomery")
MainStates1 <- filter(MainStates, region %in% c("district of columbia") |
                        (region == "virginia" & subregion %in% subs ) |
                        (region == "maryland" & subregion %in% subs1))
save(MainStates1, file = here("data/MainStates1.RData"))

cols <- brewer.pal(8, "Dark2") %>% rep(4)
# g2 <- ggplot() +
#   geom_polygon( data=MainStates1, aes(x=long, y=lat, group = group),
#                 color="grey50",fill = "white") +
#   geom_point(data = loc, aes(x = Longitude, y = Latitude, colour= ID), alpha = 0.3) +
#   scale_color_manual(values = cols) +
#   theme(legend.position = "none") +
#   xlab("Longitude") + ylab("Latitude")
# ggsave(g2, file = here("plots/gestdc-commutes.png"), width = 4, height = 4, units = "in")


# downsampled
t1 <- tripdata1 %>%
  mutate(date = date(datetime)) %>%
  group_by(., date, ID, Trip) %>%
  mutate(row = row_number()) %>%
  filter(row %% 20 == 0) %>%
  dplyr::select(ID, Longitude, Latitude) %>%
  as.data.frame()

g2 <- ggplot() +
  geom_polygon( data=MainStates1, aes(x=long, y=lat, group = group),
                color="grey50",fill = "white") +
  geom_point(data = t1, aes(x = Longitude, y = Latitude, colour= ID), alpha = 0.3) +
  scale_color_manual(values = cols) +
  theme(legend.position = "none") +
  xlab("Longitude") + ylab("Latitude")
# ggsave(g2, file = here("plots/gestdc-commutes-ds.png"), width = 4, height = 4, units = "in")
g2




save(MainStates1, t1, file = here("results/gpsplotdata.RData"))
