# file to run all

library(here)

# get lat long for each trip
# source(here("rcode/get-GMU_commute.R"))

# check-commute-data.R: Check/analyze gpslatlon.RData.

# get unique lat/lon
# source(here("rcode/unique_commutes_df.R"))

# get roadiness map/raster
# source(here("rcode/roadiness_1km_plot.R"))

# align roadiness with lat/lon from commutes
# source(here("rcode/commutes_to_gridcell.R"))

# get road types
# source(here("rcode/find-roads.R"))


########### Files usual to have to rerun
# merge GESTDC and roadiness
# source(here("rcode/roadiness_dataset.R"))

# get VA data
# source(here("rcode/clean-va.R"))

# get weather data (not all necessary)
# source(here("rcode/new-weather.R"))
# source(here("rcode/weather.R"))

# add in PM data to final dataset
# source(here("rcode/adjust-pm.R"))

# fix varialbes based on EDA
# source(here("rcode/fix-data.R"))

