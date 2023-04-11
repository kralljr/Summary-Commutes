library(tidyverse)
library(sf)


# from roadiness_1km_plot from Lucas
load(here("data/plot-dat.RData"))
# from new-roadness
load(here("data/gridpolygon.RData"))

# roadiness_in.r <- rasterFromXYZ( roadiness_in, crs = p4s)
# roadiness.r <- roadiness_in.r$leng.distm2_scale
# roadiness.sf <- st_as_sf( rasterToPolygons( roadiness.r))
grid500 <- st_as_sf(gridPolygon500)
grid1 <- st_as_sf(gridPolygon1)

makeplot <- function(data, var) {
  if(var == "HW") {
    g1 <- ggplot( ) +
      geom_sf( data = data,
               aes( fill = leng.distm2_hw_scale),
               color = NA)
  } else {
    g1 <- ggplot( ) +
      geom_sf( data = data,
               aes( fill = leng.distm2_loc_scale),
               color = NA)
  }
  g1 +
    geom_sf( data = counties_use.sf,
             color = 'white',
             size = .5,
             fill = NA) +
    geom_sf( data = states.us.sf,
             color = 'white',
             size = 1.25,
             fill = NA) +
    scale_fill_viridis( name = "Density",
                        direction = -1) +
    expand_limits( fill = 0) +
    theme_minimal() +
    theme( axis.text = element_blank(),
           legend.direction = 'horizontal',
           legend.position = 'bottom',
           legend.text = element_text( size = 14),
           legend.title = element_text( size = 18),
           panel.grid = element_blank())
}

hw1 <- makeplot(grid1, "HW")
hw1
ggsave(here("figures/map1hw.png"))
loc1 <- makeplot(grid1, "Loc")
loc1
ggsave(here("figures/map1loc.png"))
hw500 <- makeplot(grid500, "HW")
hw500
ggsave(here("figures/map500hw.png"))
loc500 <- makeplot(grid500, "Loc")
loc500
ggsave(here("figures/map500loc.png"))

save(hw1, loc1, hw500, loc500, file = here("data/maps.RData"))
