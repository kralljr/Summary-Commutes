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
    - something wrong here
    
6. roadiness_dataset.R: Merge with full GEST-DC commute dataset

    - Input: points_gricell.Rdata
    - Creates: roadiness_commutes.Rdata
