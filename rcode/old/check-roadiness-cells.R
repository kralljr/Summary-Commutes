# Check grid cell
library(tidyverse)
library(here)
library(maps)


# all missing grid cells
# still don't know why!!!
dat <- filter(rpm, is.na(leng.distm2_scale))


map("state", c("virginia", "maryland", "pennsylvania", "delaware"))
axis(1)
axis(2)

points(dat$Longitude, dat$Latitude, col = "red")


