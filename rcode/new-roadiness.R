# new roadiness measures

library(fst)
library(here)
library(tidyverse)
library( sf)
library( data.table)
library( viridis)
# need raster package
library(raster)

library( sp)
library( spatialEco)
library( proj4)




# define coordinate reference system
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"





#####
# Commute data
#
# load locations of commutes
load(here("data/unique_commutes_df.Rdata"))
load(here("data/gpslatlon.RData"))
# restrict to complete GPS data
gridPoints <- filter(gpslatlon$dat, missing == 1) %>% data.frame()
## ======================================================
#  create Spatial Points object from commute data
## ======================================================
# extract longitude and latitude (in that order)
xy <- gridPoints[, c("Longitude", "Latitude")]
gridPoints <- SpatialPoints(coords = xy, proj4string = CRS(p4s))

# project longitude / latitude coordinates using coordinate reference system defined in roadiness_1km_plot.R:
gridPoints@coords <- proj4::project(coordinates(gridPoints), p4s)











## ======================================================
#  500m
## ======================================================
dat500 <- read_fst(here("data/roadiness_500m_hw_loc.fst"))

# convert to raster file
dat500b <- rasterFromXYZ( dat500, crs = p4s)


# just grab leng.distm2, which is what I used in the paper for roadiness
gridPolygon <- rasterToPolygons( dat500b)
gridPolygon500 <- gridPolygon
gridPolygon@data$grid_cell500 <- 1:nrow(gridPolygon)


## ======================================================
#  assign our commute points to grid cells
## ======================================================
# find which polygons our lat/lon points intersect:
pointinPolygon <- spatialEco::point.in.poly(gridPoints, gridPolygon, sp = TRUE)

# transform coordinates projection
pointinPolygon@coords <- proj4::project(coordinates(pointinPolygon), p4s,
                                        inverse = TRUE)

# convert to data frame, clean up data frame, and save:
points_gridcell <- as.data.frame(pointinPolygon)
points_gridcell <- points_gridcell[,-1]%>%
  rename(Longitude = "X1",
         Latitude = "X2")

points_gridcell500 <- points_gridcell%>%
  relocate(c("Longitude", "Latitude"), c(1,2))










## ======================================================
#  1km
## ======================================================
dat1km <- read_fst(here("data/roadiness_1km_hw_loc.fst"))

# convert to raster file
dat1kmb <- rasterFromXYZ( dat1km, crs = p4s)


# just grab leng.distm2, which is what I used in the paper for roadiness
gridPolygon <- rasterToPolygons( dat1kmb)
gridPolygon1 <- gridPolygon
gridPolygon@data$grid_cell <- 1:nrow(gridPolygon)


## ======================================================
#  assign our commute points to grid cells
## ======================================================
# find which polygons our lat/lon points intersect:
pointinPolygon <- spatialEco::point.in.poly(gridPoints, gridPolygon, sp = TRUE)

# transform coordinates projection
pointinPolygon@coords <- proj4::project(coordinates(pointinPolygon), p4s,
                                        inverse = TRUE)

# convert to data frame, clean up data frame, and save:
points_gridcell <- as.data.frame(pointinPolygon)
points_gridcell <- points_gridcell[,-1]%>%
  rename(Longitude = "X1",
         Latitude = "X2")

points_gridcell <- points_gridcell%>%
  relocate(c("Longitude", "Latitude"), c(1,2))

# save(points_gridcell, points_gridcell500, file = here("data/new_roadiness.Rdata"))



####
## check
# old
load( here("data/points_gricell.Rdata"))

old <- points_gridcell

load(here("data/new_roadiness.Rdata"))

# check lat/lon
# all.equal(old$Longitude, points_gridcell$Longitude)
# all.equal(old$Latitude, points_gridcell$Latitude)
# all.equal(old$Latitude, points_gridcell500$Latitude)
# all.equal(old$Longitude, points_gridcell500$Longitude)

# highly corr but not perfectly corr?
cor(points_gridcell$leng.distm2_loc_scale, old$leng.distm2_scale, use = "pair")

# high correlation based on resolution
cor(points_gridcell$leng.distm2_loc_scale, points_gridcell500$leng.distm2_loc_scale, use = "pair")
cor(points_gridcell$leng.distm2_hw_scale, points_gridcell500$leng.distm2_hw_scale, use = "pair")

# low correlation based on road type
cor(points_gridcell$leng.distm2_loc_scale, points_gridcell$leng.distm2_hw_scale, use = "pair")
cor(points_gridcell500$leng.distm2_loc_scale, points_gridcell500$leng.distm2_hw_scale, use = "pair")



# save only needed columns
points_gridcell1km <- dplyr::select(points_gridcell, 1 : 2, leng.distm2_loc_scale, leng.distm2_hw_scale, grid_cell)
points_gridcell500 <- dplyr::select(points_gridcell500, 1 : 2, leng.distm2_loc_scale,
                                    leng.distm2_hw_scale, grid_cell500) %>%
  rename(leng.distm2_loc_scale500 = leng.distm2_loc_scale,
         leng.distm2_hw_scale500 = leng.distm2_hw_scale)
#double check
all.equal(points_gridcell1km$Longitude, points_gridcell500$Longitude)
all.equal(points_gridcell1km$Latitude, points_gridcell500$Latitude)

points_gridcell500 <- dplyr::select(points_gridcell500, -1, -2)
points_gridcell <- cbind(points_gridcell1km, points_gridcell500)

save(gridPolygon500, gridPolygon1, file = here("data/gridpolygon.RData"))

save(points_gridcell, file = here("data/points_gricell-new.RData"))
