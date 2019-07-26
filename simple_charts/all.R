setwd("C:/Users/Kate Newhart/Dropbox/Newhart ADPCA Paper/R")
library(xts)
library(mvMonitoringv2)
library(ADPCA)

# Clean process data for initial training
dataLocation <- "C:/Users/Kate Newhart/odrive/Mines/Data/MP_SBMBR_data/"
change <- 4
testingDay <- as.Date("2018-01-04")-change
nDays <- 6-change
keyword <- "training"
rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",testingDay,sep="")]
# graph_rawData(rawData, paste0("case_studies/train 7d/",keyword))

# After moving backwards from December 31, 2017, there were very few instances were all process variables were stable for more than 48 hours
# Let's start at 2017-11-28 and see what happens
train.data <- rawData
train.data <- uniquenessCheck(train.data, N=6) # Must have more than 1 unique value
nDays <- 23
testingDay <- as.Date(index(train.data)[1]) + nDays
rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",testingDay,sep="")]
train.and.test.data <- rawData
# graph_rawData(train.and.test.data, paste0("case_studies/train 6d/",keyword))
train.and.test.data <- train.and.test.data[,which(colnames(train.and.test.data) %in% colnames(train.data))]
### Classify real faults ###
library(lubridate)
real.faults <- interval(as.POSIXct("2018-01-05 09:11:13", tz="MST"), as.POSIXct("2018-01-05 11:08:17", tz="MST")) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-11 11:55:11", tz="MST"), as.POSIXct("2018-01-11 12:46", tz="MST"))) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-18 10:28:58", tz="MST"), as.POSIXct("2018-01-19 12:20:00", tz="MST")))  # BR Overdose/Relaxation mode
# Alarms from 1/22-1/25 not considered faults because not true out of control conditions, rather not necessarily "normal"

plot(as.zoo(train.and.test.data["2018-01-11 12:30/2018-01-11 13:00"][,c(1,4)]))



###################################
## Option 2: MCUSUM/MEWMA
###################################
library(MSQC) ## for MEWMA and MCUSUM
library(qcc) ## for univariate monitoring

## data must be a matrix or data frame
fault1A <- as.data.frame(train.and.test.data[1:1000,1:10])
# fault1A <- matrix(c(apply(train.and.test.data,2,as.numeric)),nrow=nrow(train.and.test.data), ncol=ncol(train.and.test.data))
str(fault1A)

## training/test data
uniqueness.error <- TRUE
train1A <- fault1A[1:nrow(train.data), ]
keep.these <- seq(1,ncol(train1A),by=1)
while(uniqueness.error == TRUE) {
  train1A <- train1A[, keep.these]
  try({
    Xmv <- mult.chart(x=train1A[1:1000,], type = "t2")
    uniqueness.error <- FALSE
  })
  keep.these <- 
  as.numeric(which(min(apply(train1A, 2, function(x) length(unique(x)))) < apply(train1A, 2, function(x) length(unique(x)))))
}

test1A <- fault1A[(nrow(train.data)+1):nrow(train.and.test.data), (colnames(fault1A) %in% colnames(train1A))]










## estimate mean and covariance using training data

S <- mult.chart(train1A, type = "t2")$covariance

## monitor test data
## MEWMA
monitor1A_mewma <- mult.chart(type = "mewma", test1A[, c("x", "y", "z")], Xmv = Xmv, S = S)
## MCUSUM
monitor1A_mcusum <- mult.chart(type = "mcusum", test1A[, c("x", "y", "z")], Xmv = Xmv, S = S)

## append results to data frame
## test statistic
test1A$mewma_t2 <- as.vector(monitor1A_mewma$t2)
## 0 if IC, 1 if OC
test1A$mewma_flag <- as.numeric(test1A$mewma_t2 > monitor1A_mewma$ucl)
## 0 if no alarm based on mewma, 1 if alarm based on MEWMA
test1A$mewma_alarm <- rep(NA, nrow(test1A)) ## placeholder
## calculate same 3 columns for MCUSUM
test1A$mcusum_t2 <- as.vector(monitor1A_mcusum$t2)
test1A$mcusum_flag <- as.numeric(test1A$mcusum_t2 > monitor1A_mcusum$ucl)
test1A$mcusum_alarm <- rep(NA, nrow(test1A)) ## placeholder

## determine whether it alarms
## code adapted from faultFilter function in mvMonitoring
faultsToTriggerAlarm <- 5
alarmCheck <- rep(1, faultsToTriggerAlarm)
if (nrow(test1A) >= faultsToTriggerAlarm) {
  for (i in faultsToTriggerAlarm:nrow(test1A)) {
    x1 <- test1A$mewma_flag[(i - faultsToTriggerAlarm + 1):i]
    if (identical(x1, alarmCheck)) {
      test1A$mewma_alarm[i] <- 1
    }
    x2 <- test1A$mcusum_flag[(i - faultsToTriggerAlarm + 1):i]
    if (identical(x2, alarmCheck)) {
      test1A$mcusum_alarm[i] <- 1
    }
  }
}

head(test1A)

##################################
## Option 3: Univariate monitoring
##################################
## training/test data
univariate_cc <- function(train, test) {
  ## matrix with same dim as test
  ## one column for each variable
  flags_ewma <- matrix(rep(0, nrow(test) * ncol(test)), ncol = ncol(test))
  flags_cusum <- matrix(rep(0, nrow(test) * ncol(test)), ncol = ncol(test))
  ## for each variable, do EWMA or CUSUM control chart
  for (var in 1:ncol(test)) {
    train_var <- train[, var]
    test_var <- test[, var]
    
    monitor_ewma <- ewma(test_var, center = mean(train_var), std.dev = sd(train_var))
    monitor_cusum <- cusum(test_var, center = mean(train_var), std.dev = sd(train_var))
    ## if it exceeds CL, change flag for that obs. from 0 to 1
    flags_ewma[unlist(monitor_ewma$violations), var] <- 1
    flags_cusum[unlist(monitor_cusum$violations), var] <- 1
  }
  ## if any of the flags are 0 for that observation, overall flag is 1
  test$ewma_flag <- as.numeric(rowSums(flags_ewma) != 0)
  test$cusum_flag <- as.numeric(rowSums(flags_cusum) != 0)
  test$ewma_alarm <- rep(NA, nrow(test)) # placeholder
  test$cusum_alarm <- rep(NA, nrow(test)) # placeholder
  
  ## determine whether there is an alarm in the same way as Option 2
  faultsToTriggerAlarm <- 5
  alarmCheck <- rep(1, faultsToTriggerAlarm)
  if (nrow(test) >= faultsToTriggerAlarm) {
    for (i in faultsToTriggerAlarm:nrow(test)) {
      x1 <- test$ewma_flag[(i - faultsToTriggerAlarm + 1):i]
      if (identical(x1, alarmCheck)) {
        test$ewma_alarm[i] <- 1
      }
      x2 <- test$cusum_flag[(i - faultsToTriggerAlarm + 1):i]
      if (identical(x2, alarmCheck)) {
        test$cusum_alarm[i] <- 1
      }
    }
  }
  test
}

monitor_univariate <- univariate_cc(train = train1A[, c("x", "y", "z")], 
                                    test = test1A[, c("x", "y", "z")])
head(monitor_univariate)