# snap to roads
library(tidyverse)
library(googleway)
library(maps)
library(ggmap)
library(jsonlite)
library(httr)
library(geosphere)

# paths
load(here("data/tripdata.RData"))

pathf <- function(df) {
  paste(df$path, collapse = "|")
}

# nrow(unique(tripdata))
nroads <- 750341
# cost for roads:
# $10 / 1000 requests, each request can be 100
cost <- 10 / 1000
rrequests <- nroads / 100
rcost <- cost * rrequests
# cost for places
# $17 / 1000 requests
# probably < half of points
npts <- nroads / 2
cost <- 17 / 1000
pcost <- cost * npts


# API key
register_google("AIzaSyCwiOJhkDb4tTq_bI5mSn2wshac3N6YIRc")
skr <- set_key("AIzaSyCwiOJhkDb4tTq_bI5mSn2wshac3N6YIRc", "roads")
skp <- set_key("AIzaSyCwiOJhkDb4tTq_bI5mSn2wshac3N6YIRc", "places_details")

tripdata1 <- mutate(tripdata, path = paste0(Latitude, ",", Longitude))

t100 <- tripdata1[1 : 100, c(2, 3)]
t100 <- tripdata[10000 : 10020, c(2, 3)]
t100 <- unique(t100)
t100 <- data.frame(t100)
# ggplot(data = t100, aes(x = Longitude,y = Latitude )) +
#   geom_point(colour = "red") +
#   geom_point(data = t200, aes(x = Longitude, y = Latitude))


f100 <- google_nearestRoads(t100, lat = "Latitude", lon = "Longitude")
f100s <- google_snapToRoads(t100, lat = "Latitude", lon = "Longitude")

points1 <- unique(c(unique(f100$snappedPoints$placeId), unique(f100s$snappedPoints$placeId)))

myplace <- function(placeid) {
  #placeid <- paste(placeid, collapse = "|")
  call1 <- "https://maps.googleapis.com/maps/api/place/details/json?fields=name&place_id="
  call2 <- "&key=AIzaSyCwiOJhkDb4tTq_bI5mSn2wshac3N6YIRc"
  call <- paste0(call1, placeid, call2)
  # print(call)
  rawres <- GET(call)
  #print(rawres)
  res <- content(rawres, as="parsed")
  res$result$name
}
#myplace("ChIJZXmTmopOtokRzHl8ibJpjNo")
mp1 <- myplace(points1[1])
mp2 <- myplace(points1[2])
mp3 <- myplace(points1[3])
mp4 <- myplace(points1[4])
mp5 <- myplace(points1[5])

#
# tripdata1 <- mutate(tripdata, path = paste0(Latitude, ",", Longitude)) %>%
#   select(-c(Latitude, Longitude)) %>%
#   nest(data = path) %>%
#   mutate(paths= purrr::map(data, ~pathf(df = .))) %>%
#   unnest(paths) %>%
#   select(-data)
