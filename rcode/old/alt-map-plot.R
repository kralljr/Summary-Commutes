
# Plot commutes

p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

join2 <- join1 %>% st_as_sf(coords = c("Longitude", "Latitude"), crs=4326) %>%
  st_transform( crs=st_crs( p4s))
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
counties_use.sf <- merge( counties_use, counties.us) %>%
  st_as_sf( sf_column_name = 'geometry') %>%
  st_transform( crs = st_crs( p4s))


# get states
states.us <- USAboundaries::us_states( states = c( 'Virginia', 'Maryland',
                                                   'District of Columbia',
                                                   'West Virginia'))

states.us.sf <-
  st_transform( states.us, crs = st_crs( p4s))
## ======================================================
#  create a plot
## ======================================================
baseplot <- function(dat) {
  ggplot( ) +
    geom_sf( data = counties_use.sf,
             color = 'grey',
             size = .5,
             fill = NA) +
    geom_sf( data = dat) +
    expand_limits( fill = 0) +
    theme_minimal() +
    theme( axis.text = element_blank(),
           legend.direction = 'horizontal',
           legend.position = 'bottom',
           legend.text = element_text( size = 14),
           legend.title = element_text( size = 18),
           panel.grid = element_blank())
}

# baseplot(filter(join1, ID== "GMU1001"))

baseplot(slice(join2, 1 : 1000)) + facet_wrap(ID~ rows)

