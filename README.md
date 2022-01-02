# Forked repository

This repository is forked from Gabriella Armada's work <https://github.com/gabiarmada/Summary-Commutes>.  Please see the full description there as well as in <https://github.com/gabiarmada/Roadiness>.

# Rcode files

1. get-GMU_commute.R: Adapted from Gabi's code.  
    
    - Input: All GPS .csv files from the GEST-DC data loggers
    - Creates: gpslatlon.RData, one dataset of ID, time, latitude/longitude for each trip.
2. check-commute-data.R: Check/analyze gpslatlon.RData.
3. unique_commutes_df.R: 
   
    - Input: gpslatlon.RData
    - Creates: unique_commutes_df.RData, a list of unique latitude/longitude combinations for our data to align with roadiness data.
4. roadiness_1km_plot.R: File to create map/raster of roadiness data.  Adapted code from Lucas Henneman (uses csv file from Lucas)

    - Input: roadiness_1km.csv
    - Creates: roadinessr.RData, raster file of roadiness information
    - Creates: figure1_DMV.png, plot of roadiness for northern VA
5. commutes_to_gridcell.R: File to align roadiness with lat/lon from commutes

    - Input: roadinessr.RData, unique_commutes_df
    - Creates: points_gricell.Rdata: Roadiness for each lat/lon

6. find-roads.R: 

    - Input: gpslatlon.RData (GPS GEST DC data to Lucas), Road types from Lucas: gps_road_assigned.csv
    - Output: Revised complete GESTDC lat/lon latlon.RData, Road types: rtypes.RData
    
7. roadiness_dataset.R: Merge with full GEST-DC commute dataset

    - Input: points_gricell.Rdata, pm-cont-data-1minrti.RData (RTI adjusted PM), rtypes.RData
    - Creates: roadiness_commutes.Rdata (rtype is mode, rtype2 is mean)
    
8. clean-va.R: File to get PM for daily VA, hourly avg

    - Input: va_monitor_data.RData, GMU_RTI_Metadata-loq-23oct19.xlsx (dates for GEST DC), 
    - Output: vah.RData (hourly + seasonal/hourly, separate dataset with obs hourly: vah0), va24.RData (24 avg)
    
9. weather.R: File to clean weather data

    - Input: API key, GEST DC dates (gestdc-dates.RData)
    - Output: weather-cleaned.RData

10. adjust-pm.R: File to incorporate potential adjustment variables (daily PM, weather etc)

    - Input: roadiness_commutes.Rdata, vah.RData (hourly averages VA PM, hourly+season, observed), va24.RData (daily PM), weather-cleaned.RData (weather data)
    - Output: rcomm.RData
    
X. check-roadiness-cells.R: Checking file with maps to verify NA cells
X. roadiness-lm.Rmd: Preliminary LMM for commute data
