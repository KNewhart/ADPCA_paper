library(ADPCA)
# Clean process data for initial training
dataLocation <- "C:/Users/Kate Newhart/odrive/Mines/Data/MP_SBMBR_data/"
testingDay <- as.Date("2017-11-28")
nDays <- 1
keyword <- "testing"
rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",testingDay,sep="")]
# graph_rawData(rawData, paste0("results/",keyword))
# After moving backwards from December 31, 2017, there were very few instances were all process variables were stable for more than 48 hours
# Let's start at 2017-11-28 and see what happens