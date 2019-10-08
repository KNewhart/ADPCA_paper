### Preliminaries ###
library(xts)
library(mvdalab)

source("src/constants.R")

# Starting day
dayOne <- dayN - 2*rollingWindowDays

# Range of dates to compile
daysToTest <- as.numeric(difftime(dayN, dayOne)) + 1

# Move into folder to save results
setwd(paste0("results/",folder))

for(i in 1:length(rollingWindowDays)) {
  # Compile training and testing data
  rawData <- loadandcleanDBF(dataLocation, dayOne[i]+daysToTest[i], daysToTest[i])
  rawData <- xts(rawData[,-1], order.by = rawData[,1])
  rawData <- rawData[paste("/",dayOne[i]+daysToTest[i],sep="")]
  train.and.test.data <- rawData
  train.data <- uniquenessCheck(train.and.test.data[paste0("/",dayOne[i]+rollingWindowDays[i]-1)], N=rollingWindowDays[i])
  test.data <- train.and.test.data[paste0(dayOne[i]+rollingWindowDays[i],"/")]
  test.data <- test.data[,colnames(train.data)]
  
  # Generate T2 graph
  mewma(train.data[1:10000,])
}