# Assign our lat/long GPS points to roadiness grid cells


library( sp)
library( raster)
library( spatialEco)
library( proj4)
library( dplyr)
library(here)

# define coordinate reference system (from roadiness_1km_plot.R)
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"



# load data
# from lucas: roadiness_1km_plot.R
load( here("data/roadinessr.RData"))
# jrk recreation: check with gabi
load(here("data/unique_commutes_df.Rdata"))



## ======================================================
#  load unique commutes df
## ======================================================
# gridPoints <- data.frame(unique_commutes_df)

# try with full data (maybe issues?)
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
#  create Spatial Polygons object from roadiness.r
## ======================================================
gridPolygon <- rasterToPolygons( roadiness.r)
gridPolygon@data$grid_cell <- 1:nrow(gridPolygon)


## ======================================================
#  assign our commute points to grid cells
## ======================================================
# find which polygons our lat/lon points intersect:
pointinPolygon <- spatialEco::point.in.poly(gridPoints, gridPolygon, sp = TRUE)

# transform coordinates projection
pointinPolygon@coords <- proj4::project(coordinates(pointinPolygon), p4s, inverse = TRUE)

# convert to data frame, clean up data frame, and save:
points_gridcell <- as.data.frame(pointinPolygon)
points_gridcell <- points_gridcell[,-1]%>%
                      rename(Longitude = "X1",
                             Latitude = "X2")

points_gridcell <- points_gridcell%>%
                      relocate(c("Longitude", "Latitude"), c(1,2))

# keep only points in NOVA
points_gridcell <- filter(points_gridcell, !is.na(leng.distm2_scale))

save(points_gridcell, file = here("data/points_gricell.Rdata"))
