library(weatherData)
library(data.table)
library(plyr)
library(AnomalyDetection)

# Collect data
sfo = getWeatherForDate("SFO", "2014-01-01", end_date="2015-01-02", 
                        opt_detailed=T,
                        opt_custom_columns=T, custom_columns=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))

# Format Date
sfo$DateUTC <- format(strptime(sfo$DateUTC, "%Y-%m-%d"), "%Y-%m-%d")
# Save the data
write.csv(sfo, file = "/Users/Shank/Desktop/WeatherData.csv")

cols <- c("TemperatureC", "Dew_PointC", "Humidity", "Sea_Level_PressurehPa", "VisibilityKm", "Wind_Direction", "Wind_SpeedKm_h", "Gust_SpeedKm_h", "Precipitationmm", "WindDirDegrees", "DateUTC")
tr <- sfo[, cols]

tr <- data.table(tr)

tr$TemperatureC[which(tr$TemperatureC == "N/A")] <- 0
tr$Dew_PointC[which(tr$Dew_PointC == "N/A")] <- 0

# Categorical
#tr$Wind_Direction[which(tr$Wind_Direction == "Calm")] <- 4
#tr$Wind_Direction[is.na(tr$Wind_Direction)] <- 0
#tr$Wind_Direction <- strtoi(tr$Wind_Direction)

tr$Wind_SpeedKm_h[which(tr$Wind_SpeedKm_h == "Calm")] <- 4
tr$Wind_SpeedKm_h <- as.double(tr$Wind_SpeedKm_h)

tr$Gust_SpeedKm_h[is.na(tr$Gust_SpeedKm_h)] <- 0
tr$Gust_SpeedKm_h[which(tr$Gust_SpeedKm_h == "-")] <- 0
tr$Gust_SpeedKm_h <- as.double(tr$Gust_SpeedKm_h)

tr$Precipitationmm[which(tr$Precipitationmm == "N/A")] <- 0
tr$Precipitationmm <- as.double(tr$Precipitationmm)

train <- tr[,.(
  temp = mean(TemperatureC, na.rm=T),
  dewPoint = mean(Dew_PointC, na.rm=T),
  humidity = mean(Humidity, na.rm=T),
  seaLevelPressure = mean(Sea_Level_PressurehPa, na.rm=T),
  visibility = mean(VisibilityKm, na.rm=T),
  #windDirection = mean(Wind_Direction, na.rm=T),
  windSpeed = mean(Wind_SpeedKm_h, na.rm=T),
  gustSpeed = mean(Gust_SpeedKm_h, na.rm=T),
  percipitation = mean(Precipitationmm, na.rm=T),
  windDeg = mean(WindDirDegrees, na.rm=T)
), DateUTC]

train <- data.frame(train)

