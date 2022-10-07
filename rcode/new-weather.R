# new weather data
weat <- read.csv("~/Dropbox/CommuteVar/data/3103782.csv")


cn <- colnames(weat)
keeeps <- c("HourlyDewPointTemperature", "HourlyPrecipitation", "HourlyRelativeHumidity",
"HourlyWindSpeed", "HourlyWindDirection", "HourlyWetBulbTemperature",
"HourlyDryBulbTemperature")
