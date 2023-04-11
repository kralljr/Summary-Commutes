library( sf)
library( data.table)
library(tidyverse)
library( viridis)
# add here to fixfile path
library(here)
# need raster package
library(raster)


# define coordinate reference system
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

## ======================================================
#  read in the data
## ======================================================
file.out.csv <- here("data/roadiness_1km.csv")
roadiness_in <- fread( file.out.csv)

# convert to raster file
roadiness_in.r <- rasterFromXYZ( roadiness_in, crs = p4s)
plot( roadiness_in.r)

# just grab leng.distm2, which is what I used in the paper for roadiness
roadiness.r <- roadiness_in.r$leng.distm2_scale
plot( roadiness.r)
save(roadiness.r, file = here("data/roadinessr.RData"))

# convert to sf object for easy plotting
roadiness.sf <- st_as_sf( rasterToPolygons( roadiness.r))

## ======================================================
#  retrieve spatial data about the counties
# (these are a list of DMV counties I got from Wikipedia)
## ======================================================
counties_use <- data.table( state_abbr = c( rep( 'DC', 1),
                                            rep( 'MD', 5),
                                            rep( 'VA', 16),
                                            rep( 'WV', 1)),
                            name = c( 'District of Columbia',
                                      # MD
                                      'Calvert',
                                      'Charles',
                                      'Frederick',
                                      'Montgomery',
                                      "Prince George's",
                                      ####
                                      'Alexandria',
                                      'Arlington',
                                      'Clarke',
                                      'Culpeper',
                                      'Fairfax',
                                      'Falls Church',
                                      'Fauquier',
                                      'Fredericksburg',
                                      'Loudoun',
                                      'Manassas',
                                      'Manassas Park',
                                      'Prince William',
                                      'Rappahannock',
                                      'Spotsylvania',
                                      'Stafford',
                                      'Warren',
                                      'Jefferson'))
# note: had to install USA boundaries from github
counties.us <- USAboundaries::us_counties( )
with(counties.us, table(state_name, state_name))
counties.us <- counties.us[, -13]
counties_use.sf <- merge( counties_use, counties.us) %>%
  st_as_sf( sf_column_name = 'geometry') %>%
  st_transform( crs = st_crs( p4s))


# get states
states.us <- USAboundaries::us_states( states = c( 'Virginia', 'Maryland',
                                                   'District of Columbia',
                                                   'West Virginia'))

states.us.sf <-
  st_transform( states.us, crs = st_crs( p4s)) %>%
  st_crop( roadiness.sf)

## ======================================================
#  create a plot
## ======================================================
DMV_roadiness_1km.gg <-
  ggplot( ) +
  geom_sf( data = roadiness.sf,
           aes( fill = leng.distm2_scale),
           color = NA) +
  geom_sf( data = counties_use.sf,
           color = 'white',
           size = .5,
           fill = NA) +
  geom_sf( data = states.us.sf,
           color = 'white',
           size = 1.25,
           fill = NA) +
  scale_fill_viridis( name = "Roadiness",
                      direction = -1) +
  expand_limits( fill = 0) +
  theme_minimal() +
  theme( axis.text = element_blank(),
         legend.direction = 'horizontal',
         legend.position = 'bottom',
         legend.text = element_text( size = 14),
         legend.title = element_text( size = 18),
         panel.grid = element_blank())

print( DMV_roadiness_1km.gg)
ggsave( here('plots/figure1_DMV.png'),
        DMV_roadiness_1km.gg,
        width = 5, heigh = 5, units = 'in')


save(counties_use.sf, states.us.sf, file = here("data/plot-dat.RData"))























