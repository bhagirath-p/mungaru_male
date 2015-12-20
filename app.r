library(weatherData)
library(ggplot2)
library(data.table)
library(plyr)
library(h2o)

h2o.init(nthreads=-1, max_mem_size = '1g')
rfHex <- h2o.loadModel('/Users/Shank/Desktop/Hackathon/Model1/timeModel.hex')
load("~/Desktop/Hackathon/Data.RData")

te<-fread("/Users/Shank/Desktop/Hackathon/gen.csv")
test <- te[,.(
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
  windDegMax = max(WindDirDegrees, na.rm=T),

  windDegMeanAct = mean(WindDegMeanAct, na.rm=T)
), DateUTC]
test <- data.frame(test)

dates <- subset(test, select=c(1))
test <- subset(test, select=-c(1))
test <- sweep(test, MARGIN=2, train_mean, FUN = '-')
test <- sweep(test, MARGIN=2, train_sd, FUN = '/')
test1 <- test
test <- subset(test, select=-c(42))

testHex<-as.h2o(test, destination_frame="test.hex")

predictions<-as.data.frame(h2o.predict(rfHex, testHex))
predictions
test1
res <- abs(data.frame(test1$windDegMeanAct)-predictions)
res
res <- cbind(dates, res)
res <- data.frame(res)
res <- res[which(res$test.windDegMeanAct > 1.2),]
write.csv(res, '/Users/Shank/Desktop/Hackathon/result.csv')
