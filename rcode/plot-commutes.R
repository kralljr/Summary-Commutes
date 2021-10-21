# plot commute locations

library(tidyverse)
library(maps)
library(here)
library(RColorBrewer)

# load data
load(here("data/tripdata.RData"))

# remove PA
tripdata1 <- filter(tripdata, ID != "GMU1026")
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
t1 <- group_by(tripdata1, ID, Trip) %>%
  mutate(row = row_number()) %>%
  filter(row %% 100 == 0) %>%
  select(ID, Longitude, Latitude) %>%
  as.data.frame()

g2 <- ggplot() +
  geom_polygon( data=MainStates1, aes(x=long, y=lat, group = group),
                color="grey50",fill = "white") +
  geom_point(data = t1, aes(x = Longitude, y = Latitude, colour= ID), alpha = 0.3) +
  scale_color_manual(values = cols) +
  theme(legend.position = "none") +
  xlab("Longitude") + ylab("Latitude")
ggsave(g2, file = here("plots/gestdc-commutes-ds.png"), width = 4, height = 4, units = "in")

