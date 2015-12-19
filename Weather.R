library(weatherData)
library(data.table)
library(plyr)
library(AnomalyDetection)
library(h2o)

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
tr$TemperatureC <- as.double(tr$TemperatureC)

tr$Dew_PointC[which(tr$Dew_PointC == "N/A")] <- 0
tr$Dew_PointC <- as.double(tr$Dew_PointC)

tr$Humidity <- as.double(tr$Humidity)
#tr$Humidity[is.na(tr$Humidity)] <- 0

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
  temp = sum(TemperatureC, na.rm=T),
  tempMean = mean(TemperatureC, na.rm=T),
  tempSd = sd(TemperatureC, na.rm=T),
  tempMin = min(TemperatureC, na.rm=T),
  tempMax = max(TemperatureC, na.rm=T),

  dewPoint = sum(Dew_PointC, na.rm=T),
  dewPointMean = mean(Dew_PointC, na.rm=T),
  dewPointSd = sd(Dew_PointC, na.rm=T),
  dewPointMin = min(Dew_PointC, na.rm=T),
  dewPointMax = max(Dew_PointC, na.rm=T),

  humidity = sum(Humidity, na.rm=T),
  humidityMean = mean(Humidity, na.rm=T),
  humiditySd = sd(Humidity, na.rm=T),
  humidityMin = min(Humidity, na.rm=T),
  humidityMax = max(Humidity, na.rm=T),

  seaLevelPressure = mean(Sea_Level_PressurehPa, na.rm=T),

  visibility = sum(VisibilityKm, na.rm=T),
  visibilityMean = mean(VisibilityKm, na.rm=T),
  visibilitySd = sd(VisibilityKm, na.rm=T),
  visibilityMin = min(VisibilityKm, na.rm=T),
  visibilityMax = max(VisibilityKm, na.rm=T),

  #windDirection = mean(Wind_Direction, na.rm=T),
  
  windSpeed = sum(Wind_SpeedKm_h, na.rm=T),
  windSpeedMean = mean(Wind_SpeedKm_h, na.rm=T),
  windSpeedSd = sd(Wind_SpeedKm_h, na.rm=T),
  windSpeedMin = min(Wind_SpeedKm_h, na.rm=T),
  windSpeedMax = max(Wind_SpeedKm_h, na.rm=T),

  gustSpeed = sum(Gust_SpeedKm_h, na.rm=T),
  gustSpeedMean = mean(Gust_SpeedKm_h, na.rm=T),
  gustSpeedSd = sd(Gust_SpeedKm_h, na.rm=T),
  gustSpeedMin = min(Gust_SpeedKm_h, na.rm=T),
  gustSpeedMax = max(Gust_SpeedKm_h, na.rm=T),

  percipitation = sum(Precipitationmm, na.rm=T),
  percipitationMean = mean(Precipitationmm, na.rm=T),
  percipitationSd = sd(Precipitationmm, na.rm=T),
  percipitationMin = min(Precipitationmm, na.rm=T),
  percipitationMax = max(Precipitationmm, na.rm=T),

  windDeg = sum(WindDirDegrees, na.rm=T),
  windDegMean = mean(WindDirDegrees, na.rm=T),
  windDegSd = sd(WindDirDegrees, na.rm=T),
  windDegMin = min(WindDirDegrees, na.rm=T),
  windDegMax = max(WindDirDegrees, na.rm=T)
), DateUTC]

train <- data.frame(train)
train <- subset(train, select=-c(1))
train <- scale(train)

h2o.init(nthreads=-1, max_mem_size = '1g')
trainHex <- as.h2o(train, destination_frame="train.hex")

cols <- c("temp", "tempMean", "tempMean", "tempSd", "tempMin", "tempMax", "dewPoint", 
          "dewPointMean", "dewPointSd", "dewPointMin", "dewPointMax", "humidity", 
          "humidityMean", "humiditySd", "humidityMin", "humidityMax", "visibility", 
          "visibilityMean", "visibilitySd", "visibilityMin", "visibilityMax", "windSpeed", 
          "windSpeedMean", "windSpeedSd", "windSpeedMin", "windSpeedMax", "gustSpeed", 
          "gustSpeedMean", "gustSpeedSd", "gustSpeedMin", "gustSpeedMax", "percipitation", 
          "percipitationMean", "percipitationSd", "percipitationMin", "percipitationMax", "windDeg", 
          "windDegMean", "windDegSd", "windDegMin", "windDegMax")

target <- "windDegMean"

rfHex<-h2o.randomForest(x=cols, y=target, training_frame=trainHex, model_id="timeModel.hex", ntrees=1700, sample_rate = 0.6, verbose = TRUE)

predictions<-as.data.frame(h2o.predict(rfHex,testHex))
