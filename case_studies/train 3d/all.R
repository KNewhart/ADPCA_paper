# setwd("C:/Users/Kate Newhart/Dropbox/Newhart ADPCA Paper/R")
library(xts)
library(mvMonitoringv2)
library(ADPCA)

stateVarsBR <- c("BIO_1\\CURRENT_PHASE",
                 "BIO_2\\CURRENT_PHASE",
                 "BIO_BLOWER_1\\RUNNING",
                 "BIO_BLOWER_2\\RUNNING")
stateVarsMT <- c("MBR_1\\CURRENT_MODE",
                 "MBR_2\\CURRENT_MODE",
                 "MBR\\CURRENT_FLUX_MODE",
                 "MBR_1\\CURRENT_STATE",
                 "MBR_2\\CURRENT_STATE",
                 "MBR_1_AIR_SCOUR_VALVE\\COMMAND_1",
                 "MBR_2_AIR_SCOUR_VALVE\\COMMAND_STAT")
# Process variables from raw data that will be used for the BR and MT. 
varsBR <- c("BIO_1\\CURRENT_PHASE",
            "BIO_1\\DO\\PROCESS_VALUE",
            "BIO_2\\CURRENT_PHASE",
            "BIO_2\\DO\\PROCESS_VALUE",
            "BIO_BLOWER_1\\FLOW\\PROCESS_VALUE",
            "BIO_BLOWER_1\\RUNNING",
            "BIO_BLOWER_2\\FLOW\\PROCESS_VALUE",
            "BIO_BLOWER_2\\RUNNING",
            "SEWAGE\\FLOW\\PROCESS_VALUE",
            "SEWAGE\\LEVEL\\PROCESS_VALUE",
            "AMBIENT_TEMP\\PROCESS_VALUE",
            "BIO_1\\LEVEL\\PROCESS_VALUE",
            "BIO_1\\TEMPERATURE\\PROCESS_VALUE",
            "BIO_2\\LEVEL\\PROCESS_VALUE",
            "BIO_2\\TEMPERATURE\\PROCESS_VALUE",
            "RAS_TROUGH\\PH\\PROCESS_VALUE",
            "RAS_TROUGH\\TSS\\PROCESS_VALUE",
            "RAS_TROUGH\\TEMPERATURE\\PROCESS_VALUE")
varsMT <- c("MBR_1\\CURRENT_MODE",
            "MBR_1\\PERM_FLOW\\PROCESS_VALUE",
            "MBR_1\\PERM_PRESS\\PROCESS_VALUE",
            "MBR_2\\CURRENT_MODE",
            "MBR_2\\PERM_FLOW\\PROCESS_VALUE",
            "MBR_2\\PERM_PRESS\\PROCESS_VALUE",
            "MBR\\AIR_SCOUR_FLOW\\PROCESS_VALUE_TANK_1",
            "MBR\\AIR_SCOUR_FLOW\\PROCESS_VALUE_TANK_2",
            "MBR\\CURRENT_FLUX_MODE",
            "MBR_1\\CURRENT_STATE",
            "MBR_2\\CURRENT_STATE",
            "MBR_1\\TRANS_PRESS\\PROCESS_VALUE",
            "MBR_2\\TRANS_PRESS\\PROCESS_VALUE",
            "RAS_TROUGH\\TEMPERATURE\\PROCESS_VALUE",
            "MBR\\AIR_SCOUR_PRESSURE\\PROCESS_VALUE_1",
            "MBR\\AIR_SCOUR_PRESSURE\\PROCESS_VALUE_2",
            "MBR_1_AIR_SCOUR_VALVE\\COMMAND_1",
            "MBR_2_AIR_SCOUR_VALVE\\COMMAND_STAT",
            "MBR\\AIR_SCOUR_PRESSURE\\PROCESS_VALUE",
            "RAS_TROUGH\\DO\\PROCESS_VALUE",
            "RAS_TROUGH\\PH\\PROCESS_VALUE",
            "RAS_TROUGH\\TSS\\PROCESS_VALUE",
            "PERMEATE_TANK\\CONDUCTIVITY\\PROCESS_VALUE",
            "PERMEATE_TANK\\LEVEL\\PROCESS_VALUE",
            "BIO_2\\TSS\\PROCESS_VALUE",
            "BIO_1\\TSS\\PROCESS_VALUE",
            "MBR_1\\INF_FLOW\\PROCESS_VALUE",
            "MBR_1\\LEVEL\\PROCESS_VALUE",
            "MBR_2\\INF_FLOW\\PROCESS_VALUE",
            "MBR_2\\LEVEL\\PROCESS_VALUE",
            "PERMEATE_TANK\\TURBIDITY\\PROCESS_VALUE")

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

library(MSQC) ## for MEWMA and MCUSUM
library(qcc) ## for univariate monitoring
alarm.rate.calc <- function (FaultChecks, real.alarms) {
  alarmed.t2.obs <- which(FaultChecks$Alarm == 1)
  alarmed.t2.obs <- c(alarmed.t2.obs, which(FaultChecks$Alarm == 3))
  
  nonalarmed.t2.obs <- which(FaultChecks$Alarm == 0)
  nonalarmed.t2.obs <- c(nonalarmed.t2.obs, which(FaultChecks$Alarm == 2))
  
  alarmed.spe.obs <- which(FaultChecks$Alarm == 2)
  alarmed.spe.obs <- c(alarmed.spe.obs, which(FaultChecks$Alarm == 3))
  
  nonalarmed.spe.obs <- which(FaultChecks$Alarm == 0)
  nonalarmed.spe.obs <- c(nonalarmed.spe.obs, which(FaultChecks$Alarm == 1))
  
  
  for(i in 1:length(real.alarms)){
    oc.i <- which(index(FaultChecks)  %within% real.alarms[[i]])
    # oc.obs.n <- round(as.numeric(difftime(int_end(real.alarms[[i]]),int_start(real.alarms[[i]]), units ="mins")))
    if (i == 1) {
      oc.obs <- oc.i
      # oc.total <- oc.obs.n
    } else {
      oc.obs <- c(oc.obs, oc.i)
      # oc.total <- oc.total + oc.obs.n
    }
  }
  oc.obs.index <- index(FaultChecks)[oc.obs]
  
  ic.obs <- which(!index(FaultChecks) %in% index(FaultChecks)[oc.obs])
  ic.obs.index <- index(FaultChecks)[ic.obs]
  
  # True alarm detection rate
  true.detection.t2 <- which(alarmed.t2.obs %in% oc.obs)
  tdr.t2 <- length(true.detection.t2)/length(oc.obs)
  
  true.detection.spe <- which(alarmed.spe.obs %in% oc.obs)
  tdr.spe <- length(true.detection.spe)/length(oc.obs)
  
  # IC obs are alarmed "false alarms"
  alarmed.t2.ic.obs <- which(alarmed.t2.obs %in% ic.obs)
  fdr.t2 <- length(alarmed.t2.ic.obs)/length(ic.obs)
  
  alarmed.spe.ic.obs <- which(alarmed.spe.obs %in% ic.obs)
  fdr.spe <- length(alarmed.spe.ic.obs)/length(ic.obs)
  
  # OC observations are not alarmed "missed alarms"
  nonalarmed.t2.oc.obs <- which(nonalarmed.t2.obs %in% oc.obs)
  ficr.t2 <- length(nonalarmed.t2.oc.obs)/length(oc.obs)
  
  nonalarmed.spe.oc.obs <- which(nonalarmed.spe.obs %in% oc.obs)
  ficr.spe <- length(nonalarmed.spe.oc.obs)/length(oc.obs)
  
  return(c("TDR-T2" = tdr.t2,
           "FDR-T2" = fdr.t2, 
           "FICR-T2" = ficr.t2,
           "TDR-SPE" = tdr.spe,
           "FDR-SPE" = fdr.spe, 
           "FICR-SPE" = ficr.spe))
}

### Classify real faults ###
library(lubridate)
real.faults <- interval(as.POSIXct("2018-01-05 09:11:13", tz="MST"), as.POSIXct("2018-01-05 11:08:17", tz="MST")) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-11 11:55:11", tz="MST"), as.POSIXct("2018-01-11 14:03:16", tz="MST"))) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-18 10:28:58", tz="MST"), as.POSIXct("2018-01-19 12:20:00", tz="MST")))  # BR Overdose/Relaxation mode
# Alarms from 1/22-1/25 not considered faults because not true out of control conditions, rather not necessarily "normal"

# 
# ### Find faults ###
# graph_alarmData(test.data, "case_studies/train 6d/testing", filetype = "png")
# blah.data <- test.data
# blah.data <- blah.data["2018-01-20/2018-01-23 00:00"]
# blah.data <- as.zoo(blah.data)
# for(i in 1:ncol(blah.data)) {plot(blah.data[,i], pch=20)}
# plot(blah.data)
# abline(a=6.7, b=0)
# points(blah.data[which((blah.data < 20) && (blah.data > 0))], col="red")
# range(index(blah.data[which(blah.data > 6.7)]))


###################################
## Option 1: PCA
## Single state, non-adaptive,
## not dynamic
###################################
# Test 6 days and never update
method <- "SS PCA"
testing.stat <- "SPE"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
results.all <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method
rownames(results.all) <- rownames(alarm.results)
# test.data <- cbind(train.and.test.data[paste0(range(index(alarm.results))[1], "/", range(index(alarm.results))[2])],
# alarm.results$Alarm)



method <- "SS BR PCA"
testing.stat <- "SPE"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsBR)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
results.all <- cbind(results.all, as.data.frame(alarm.rate.calc(alarm.results, real.faults)))
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsBR)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method




method <- "SS MT PCA"
testing.stat <- "SPE"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsMT)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
results.all <- cbind(results.all, as.data.frame(alarm.rate.calc(alarm.results, real.faults)))
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsMT)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method

results.1 <- results.all

###################################
## Option 2: D-PCA
## Single state, non-adaptive,
## Dynamic
###################################
method <- "SS D-PCA"
testing.stat <- "SPE"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = TRUE,
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = TRUE, 
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method
# rownames(results.all) <- rownames(alarm.results)



method <- "SS BR D-PCA"
testing.stat <- "SPE"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsBR)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = TRUE, 
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsBR)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = TRUE, 
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method




method <- "SS MT D-PCA"
testing.stat <- "SPE"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsMT)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = TRUE, ## not dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mvMonitoringv2::mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsMT)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = TRUE, ## not dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method

results.2 <- results.all



###################################
## Option 3: AD-PCA
## Single state, Adaptive,
## Dynamic
###################################
method <- "SS AD-PCA"
testing.stat <- "SPE"
train1A_ls <- mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1440, ## updates every day
  Dynamic = TRUE, ## dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1440, ## updates every day
  Dynamic = TRUE, ## dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)

alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method


method <- "SS BR AD-PCA"
testing.stat <- "SPE"
train1A_ls <- mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsBR)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1440, ## updates every day
  Dynamic = TRUE, ## dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsBR)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1440, ## updates every day
  Dynamic = TRUE, ## dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)

alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method

method <- "SS MT AD-PCA"
testing.stat <- "SPE"
train1A_ls <- mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsMT)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1440, ## updates every day
  Dynamic = TRUE, ## dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)
alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)


testing.stat <- "T2"
train1A_ls <- mspTrain(
  data = train.and.test.data[,(colnames(train.and.test.data) %in% varsMT)],
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1440, ## updates every day
  Dynamic = TRUE, ## dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic=testing.stat
)

alarm.results <- train1A_ls$FaultChecks
alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
results.all <- cbind(results.all, alarm.results)
colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat)

results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
colnames(results.all)[length(colnames(results.all))] <- method


results.3 <- results.all



###################################
## Option 4: AD-PCA
## Multistate, Adaptive,
## Dynamic
###################################
stateGenerator <- function(data,
                           stateVars,
                           vars,
                           minObs = round((2*length(which(!(colnames(data) %in% stateVars))))^2/2),
                           # testingDay = NULL,
                           rollingWindowDays = 0) {
  data <- data[,which(colnames(data) %in% c(stateVars,vars))]
  # Determine the column numbers of the state variables
  colNo <- which(colnames(data) %in% stateVars)
  
  # Returns character string of all state combinations
  data <- cbind(data, as.numeric(do.call(paste,c(as.data.frame(data[,colNo]),sep=""))))
  colnames(data)[length(colnames(data))] <- "labelCol"
  
  # Frequency distribution of each state?
  if (rollingWindowDays == 0) {
    freqDistribution <- as.data.frame(table(data$labelCol))
  } else {
    # freqDistribution <- as.data.frame(table(data$labelCol[paste((testingDay-rollingWindowDays),"/",(testingDay-1),sep="")]))
    freqDistribution <- as.data.frame(table(data$labelCol[paste(as.Date(index(data)[1]),"/",(as.Date(index(data)[1]) + rollingWindowDays),sep="")]))
    
  }
  
  # Is the frequency of each state within the subset greater than the minimum during the training period?
  statesToKeep <- as.numeric(as.character(freqDistribution[which(freqDistribution[,2] >= minObs),1]))
  
  # Test to ensure at least one state is included
  if (identical(statesToKeep, numeric(0))) {
    print("Insufficient datapoints to train. Increase rolling window size.")
    # return(data)
  } else {
    # Only keep the states with sufficient datapoints
    dataIndex <- which(data$labelCol %in% statesToKeep)
    dataNew <- data[dataIndex,-colNo]
    dataNew <- na.omit(dataNew)
    dataNew <- dataNew[apply(dataNew, 1, function(x) all(is.finite(x))),]
    data_ls <- list()
    for (i in 1:length(statesToKeep)) {
      dataHolder <- dataNew[which(dataNew$labelCol == statesToKeep[i]),]
      if (rollingWindowDays == 0) {
        data_ls <- c(data_ls, list(dataHolder))
      } else {
        data_ls <- c(data_ls, list(uniquenessCheck(dataHolder)))
      }
      
    }
    return(data_ls)
  }
  
  
}

#### MS BR ####

train.and.test.data.2 <- rawData
train.and.test.data.2 <- stateGenerator(data = train.and.test.data.2,
                                        stateVars = stateVarsBR, 
                                        vars=varsBR,
                                        # testingDay = testingDay, 
                                        rollingWindowDays = 6-change)

method <- "MS BR AD-PCA"
for(i in 1:length(train.and.test.data.2)) {
  testing.stat <- "SPE"
  train1D_ls <- mspTrain(
    data = train.and.test.data.2[[i]][,-ncol(train.and.test.data.2[[i]])],
    trainObs = nrow(train.and.test.data.2[[i]][paste(range(index(train.data)),collapse="/")]),
    labelVector = rep(1, nrow(train.and.test.data.2[[i]])), ## one br state
    updateFreq = 1440, 
    Dynamic = TRUE, 
    # lagsIncluded = 0, ## don't include lagged variables
    faultsToTriggerAlarm = 3,
    statistic=testing.stat
  )
  alarm.results <- train1D_ls$FaultChecks
  alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
  results.all <- cbind(results.all, alarm.results)
  colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat,i)
  
  testing.stat <- "T2"
  lets.try.something <- c(ncol(train.and.test.data.2[[i]]))
  error.index <- "EIGEN ERROR"
  while(error.index == "EIGEN ERROR") {
    try(expr = {
      train1D_ls <- mspTrain(
        data = train.and.test.data.2[[i]][,-lets.try.something],
        trainObs = nrow(train.and.test.data.2[[i]][paste(range(index(train.data)),collapse="/")]),
        labelVector = rep(1, nrow(train.and.test.data.2[[i]])), ## one br state
        updateFreq = 1440, 
        Dynamic = TRUE, 
        # lagsIncluded = 0, ## don't include lagged variables
        faultsToTriggerAlarm = 3,
        statistic=testing.stat
      )
      error.index <- "FINISHED"
      }, silent=FALSE)
    if(error.index == "EIGEN ERROR") {
      remove.index <- min(apply(train.and.test.data.2[[i]][,-lets.try.something],2,function(x) length(unique(x))));
      lets.try.something <- c(lets.try.something, which(apply(train.and.test.data.2[[i]],2,function(x) length(unique(x))) <= remove.index))
    }
  }
  
             
  
  alarm.results <- train1D_ls$FaultChecks
  alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
  results.all <- cbind(results.all, alarm.results)
  
  results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
  results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
  colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat,i)
}
# Weight results by state frequencies
new.cols <- grep(paste(method),colnames(results.all))
n.testing.obs <- unlist(lapply(train.and.test.data.2, function(x) nrow(x[paste0(range(index(train.data))[2],"/")])))
summary.results <- matrix(NA, nrow=nrow(results.all), ncol=length(n.testing.obs))
# Multiply each experimental result by the population frequency to weigh the results of each experiment
for(i in 1:length(n.testing.obs)) {
  summary.results[,i] <- results.all[,new.cols[i]]*n.testing.obs[i]/sum(n.testing.obs)
}
alarm.results.sum <- apply(summary.results,1,sum)
results.all <- cbind(results.all[,-new.cols], alarm.results.sum)
colnames(results.all)[length(colnames(results.all))] <- method

results.4.br <- results.all

#### MS MT ####
train.and.test.data.2 <- rawData
train.and.test.data.2 <- stateGenerator(data = train.and.test.data.2,
                                        stateVars = stateVarsMT, 
                                        vars=varsMT,
                                        # testingDay = testingDay, 
                                        rollingWindowDays = 6-change)

method <- "MS MT AD-PCA"
for(i in 1:length(train.and.test.data.2)) {
  testing.stat <- "SPE"
  train1D_ls <- mspTrain(
    data = train.and.test.data.2[[i]][,-ncol(train.and.test.data.2[[i]])],
    trainObs = nrow(train.and.test.data.2[[i]][paste(range(index(train.data)),collapse="/")]),
    labelVector = rep(1, nrow(train.and.test.data.2[[i]])), ## one br state
    updateFreq = 1440, 
    Dynamic = TRUE, 
    # lagsIncluded = 0, ## don't include lagged variables
    faultsToTriggerAlarm = 3,
    statistic=testing.stat
  )
  alarm.results <- train1D_ls$FaultChecks
  alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
  results.all <- cbind(results.all, alarm.results)
  colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat,i)
  
  testing.stat <- "T2"
  lets.try.something <- c(ncol(train.and.test.data.2[[i]]))
  error.index <- "EIGEN ERROR"
  while(error.index == "EIGEN ERROR") {
    try(expr = {
      train1D_ls <- mspTrain(
        data = train.and.test.data.2[[i]][,-lets.try.something],
        trainObs = nrow(train.and.test.data.2[[i]][paste(range(index(train.data)),collapse="/")]),
        labelVector = rep(1, nrow(train.and.test.data.2[[i]])), ## one br state
        updateFreq = 1440, 
        Dynamic = TRUE, 
        # lagsIncluded = 0, ## don't include lagged variables
        faultsToTriggerAlarm = 3,
        statistic=testing.stat
      )
      error.index <- "FINISHED"
    }, silent=FALSE)
    if(error.index == "EIGEN ERROR") {
      remove.index <- min(apply(train.and.test.data.2[[i]][,-lets.try.something],2,function(x) length(unique(x))));
      lets.try.something <- c(lets.try.something, which(apply(train.and.test.data.2[[i]],2,function(x) length(unique(x))) <= remove.index))
    }
  }
  
  
  
  alarm.results <- train1D_ls$FaultChecks
  alarm.results <- as.data.frame(alarm.rate.calc(alarm.results, real.faults))
  results.all <- cbind(results.all, alarm.results)
  
  results.all[c(1:3),length(colnames(results.all))-1] <- results.all[c(1:3),length(colnames(results.all))]
  results.all <- as.data.frame(results.all[,c(1:(length(colnames(results.all))-1))])
  colnames(results.all)[length(colnames(results.all))] <- paste(method,testing.stat,i)
}
# Weight results by state frequencies
new.cols <- grep(paste(method),colnames(results.all))
n.testing.obs <- unlist(lapply(train.and.test.data.2, function(x) nrow(x[paste0(range(index(train.data))[2],"/")])))
summary.results <- matrix(NA, nrow=nrow(results.all), ncol=length(n.testing.obs))
# Multiply each experimental result by the population frequency to weigh the results of each experiment
for(i in 1:length(n.testing.obs)) {
  summary.results[,i] <- results.all[,new.cols[i]]*n.testing.obs[i]/sum(n.testing.obs)
}
alarm.results.sum <- apply(summary.results,1,sum)
results.all <- cbind(results.all[,-new.cols], alarm.results.sum)
colnames(results.all)[length(colnames(results.all))] <- method

results.4.mt <- results.all

save("results.all", file="case_studies/train 3d/results.RData")





























### PRINT RESULTS ###
source("C:\\Users\\Kate Newhart\\odrive\\Mines\\Code\\printToPowerPoint.R")
data2print <-cbind(rownames(results.all),as.data.frame(round(results.all,3)))
colnames(data2print)[1] <- "Statistic"
printToPowerPoint(code.block = {
  data2print
}, presentation.path="case_studies/train 3d/", type="table", title="Jan 2018 - 3 days")
printToPowerPoint(code.block = {
  c(
    paste("Training Dates:",paste(range(index(train.data)),collapse="/")),
    paste("Testing Dates:",paste(index(train.and.test.data)[nrow(train.data)+1],index(train.and.test.data)[nrow(train.and.test.data)],collapse="/")),
    paste("Rolling Window Size:", paste(as.numeric(as.Date(range(index(train.data)))[2] - as.Date(range(index(train.data)))[1])),"days"),
    paste("Rolling Window Update: 1440 observations = 1 day"),
    paste("Number of Flags Before Fault Is Triggered: 3")
    )
}, presentation.path="case_studies/train 3d/", type="text", title="Jan 2018 - 3 days")























plot(as.zoo(train.and.test.data$`RAS_TROUGH\\TSS\\PROCESS_VALUE`),ylim=c(1750,6000),ylab="RAS TSS",type="p", pch=20, cex=.5);
rect(par("usr")[1],par("usr")[3],as.numeric(index(train.and.test.data)[nrow(train.data)]),par("usr")[4],col = scales::alpha("gray",.8));
points(as.zoo(train.and.test.data$`RAS_TROUGH\\TSS\\PROCESS_VALUE`),type="p", pch=20, cex=.5);
points(as.zoo(train.and.test.data$`RAS_TROUGH\\TSS\\PROCESS_VALUE`[oc.obs.index]), pch=20, cex=.5, col="red");
par(new=T)
plot(as.zoo(alarm.results$SPE), col=scales::alpha("red",.8), main="", xaxt="n", yaxt="n", ylab="")
axis(side=4,col.axis="red", col="red")
abline(a=as.numeric(train1A_ls$TrainingSpecs$'1'$SPE_threshold), b = 0, col="red", lty=2)
par(new=T)
plot(as.zoo(alarm.results$T2), col=scales::alpha("blue",.8),
     main="",
     ylab="",xaxt="n", yaxt="n")
axis(side=4,col.axis="blue",col.ticks = "blue", labels=FALSE)
at = axTicks(4)
mtext(side = 4, text = at, at = at, col = "blue", line = 2, cex=0.7)
abline(a=as.numeric(train1A_ls$TrainingSpecs$'1'$T2_threshold), b = 0, col="blue", lty=2)







# Plot T2 and SPE scores over process variables
par(mfrow=c(1,1))
par(mar=c(3,4,1,4))

plot(as.zoo(train.and.test.data$`BIO_2\\LEVEL\\PROCESS_VALUE`[index(alarm.results)]),
     main="", ylab="Bioreactor 2 Level (ft)", xlab="")

par(new=T)
plot(as.zoo(alarm.results$SPE), col=scales::alpha("red",.8), main="", xaxt="n", yaxt="n", ylab="")
axis(side=4,col.axis="red", col="red")

par(new=T)
plot(as.zoo(alarm.results$T2), col=scales::alpha("blue",.8),
     main="",
     ylab="",xaxt="n", yaxt="n")
axis(side=4,col.axis="blue",col.ticks = "blue", labels=FALSE)
at = axTicks(4)
mtext(side = 4, text = at, at = at, col = "blue", line = 2)


# par(mfrow=c(3,1))
for(plot.col in 1:ncol(train.and.test.data)) {
  par(mfrow=c(1,1))
  par(mar=c(3,4,1,4))
  
  plot(as.zoo(train.and.test.data[index(alarm.results),plot.col]),
       main="", ylab=colnames(train.and.test.data)[plot.col], xlab="")
  
  par(new=T)
  plot(as.zoo(alarm.results$SPE), col=scales::alpha("red",.8), main="", xaxt="n", yaxt="n", ylab="")
  axis(side=4,col.axis="red", col="red")
  
  par(new=T)
  plot(as.zoo(alarm.results$T2), col=scales::alpha("blue",.8),
       main="",
       ylab="",xaxt="n", yaxt="n")
  axis(side=4,col.axis="blue",col.ticks = "blue", labels=FALSE)
  at = axTicks(4)
  mtext(side = 4, text = at, at = at, col = "blue", line = 2)
}

# Vars of interest: "RAS_TROUGH\\TSS\\PROCESS_VALUE", "PERMEATE_TANK\\LEVEL\\PROCESS_VALUE", 
# "BIO_2\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", 
# "BIO_1\\LEVEL\\PROCESS_VALUE"
# "BIO_BLOWER_2\\FLOW\\PROCESS_VALUE", "BIO_BLOWER_1\\FLOW\\PROCESS_VALUE",
# "BIO_2\\DO\\PROCESS_VALUE", "BIO_1\\DO\\PROCESS_VALUE",
# "MBR\\BATCH_VOLUME", "MBR_1\\CURRENT_STATE"

plot.var <- c("RAS_TROUGH\\TSS\\PROCESS_VALUE", 
              # "PERMEATE_TANK\\LEVEL\\PROCESS_VALUE",
              "PERMEATE_TANK\\TURBIDITY\\PROCESS_VALUE",
              "BIO_2\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", 
              "BIO_1\\LEVEL\\PROCESS_VALUE", 
              # "BIO_BLOWER_2\\FLOW\\PROCESS_VALUE", 
              "BIO_BLOWER_1\\FLOW\\PROCESS_VALUE",
              "BIO_2\\DO\\PROCESS_VALUE", "BIO_1\\DO\\PROCESS_VALUE",
              "MBR\\BATCH_VOLUME", "MBR_1\\CURRENT_STATE",
              "MBR_2\\INF_FLOW\\PROCESS_VALUE")
plot.var.n <- which(colnames(train.and.test.data) %in% plot.var)
png("plot_test.png", res=500, units="in", width=16, height = 9)
par(mfrow=c(3,round(length(plot.var)/3)))
for(plot.col in plot.var.n) {
  # par(mfrow=c(1,1))
  par(mar=c(3,4,1,4))
  
  plot(as.zoo(train.and.test.data[index(alarm.results),plot.col]),
       main="", ylab=colnames(train.and.test.data)[plot.col], xlab="")
  
  par(new=T)
  plot(as.zoo(alarm.results$SPE), col=scales::alpha("red",.8), main="", xaxt="n", yaxt="n", ylab="")
  axis(side=4,col.axis="red", col="red")
  abline(a=as.numeric(train1A_ls$TrainingSpecs$'1'$SPE_threshold), b = 0, col="red", lty=2)
  
  par(new=T)
  plot(as.zoo(alarm.results$T2), col=scales::alpha("blue",.8),
       main="",
       ylab="",xaxt="n", yaxt="n")
  axis(side=4,col.axis="blue",col.ticks = "blue", labels=FALSE)
  at = axTicks(4)
  mtext(side = 4, text = at, at = at, col = "blue", line = 2, cex=0.7)
  abline(a=as.numeric(train1A_ls$TrainingSpecs$'1'$T2_threshold), b = 0, col="blue", lty=2)
}
dev.off()

