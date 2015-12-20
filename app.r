library(weatherData)
library(ggplot2)
library(data.table)
library(plyr)
library(h2o)

h2o.init(nthreads=-1, max_mem_size = '1g')
rfHex <- h2o.loadModel('/Users/Shank/Desktop/Hackathon/ModelNew/timeModel.hex')

test<-fread("/Users/Shank/Desktop/Hackathon/generated.csv")
test <- data.frame(test)

dates <- subset(test, select=c(1))
test <- subset(test, select=-c(1))
#test <- sweep(test, MARGIN=2, train_mean, FUN = '-')
#test <- sweep(test, MARGIN=2, train_sd, FUN = '/')

testHex<-as.h2o(test, destination_frame="test.hex")

predictions<-as.data.frame(h2o.predict(rfHex, testHex))
res <- abs(data.frame(test$windDegMean)-predictions)
res <- cbind(dates, res)
res <- data.frame(res)
res <- res[which(res$test.windDegMean > 0.8),]
write.csv(res, '/Users/Shank/Desktop/Hackathon/result.csv')
