# fix distance between obs
# snap to roads
library(tidyverse)
library(geosphere)

# paths
load(here("data/tripdata.RData"))

# Function to get GC distance (haversine)
distf <- function(lat1, long1, lat2, long2) {
  lat1 <- lat1 * pi /180
  lat2 <- lat2 * pi / 180
  long1 <- long1 * pi / 180
  long2 <- long2 * pi / 180
  difflat <- abs(lat1- lat2)
  difflong <- abs(long1 - long2)
  # haversine
  2*6378137* asin(sqrt((sin(difflat / 2)^2 + cos(lat1) * cos(lat2) * sin(difflong/2)^2)))
}



tripdata1 <- group_by(tripdata, tripid) %>%
  mutate(laglat = lag(Latitude), laglon = lag(Longitude),
         dist = distf(Latitude, Longitude, laglat, laglon),
         # unique per ID/trip
         # row = row_number(),
         # max1 = max(row)
         # id new group starts
         newgroup = ifelse(dist > 300, 1, 0),
         # fix starts
         newgroup = ifelse(is.na(dist) & is.na(newgroup), 0, newgroup),
         group = cumsum(newgroup),
         tripid2 = paste0(tripid,"-" , group)
         )  %>%
  ungroup() %>%
  dplyr::select(-c(laglat, laglon, newgroup, group))


# check new distances
tripdata2 <- group_by(tripdata1, tripid2) %>%
  mutate(laglat = lag(Latitude), laglon = lag(Longitude),
         dist = distf(Latitude, Longitude, laglat, laglon))
# none: good!
# filter(tripdata2, dist > 300)

ggplot(tripdata2, aes(x = (dist))) + geom_boxplot()



# check duplicated (>300 in the same tripid)
# max(tripdata1$dist, na.rm = T)
# try1 <- dplyr::filter(tripdata1, dist > 300)
# nrow(try1) / nrow(tripdata1)  * 100 # perc with dist > 300 m as rec by google < 1%
# dups <- try1$tripid[which(duplicated(try1$tripid))] %>% unique()
# try1 <- dplyr::filter(try1, tripid %in% dups)
# dups2 <- try1$tripid2[which(duplicated(try1$tripid2))] %>% unique()
