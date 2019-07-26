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
train.data <- rawData
train.data <- uniquenessCheck(train.data, N=1) # Must have more than 1 unique value
testingDay <- as.Date("2017-12-06")
nDays <- 2+7
rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",testingDay,sep="")]
train.and.test.data <- rawData
graph_rawData(train.and.test.data, paste0("results/",keyword))
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
    oc.i <- which(index(data)  %within% real.alarms[[i]])
    # oc.obs.n <- round(as.numeric(difftime(int_end(real.alarms[[i]]),int_start(real.alarms[[i]]), units ="mins")))
    if (i == 1) {
      oc.obs <- oc.i
      # oc.total <- oc.obs.n
    } else {
      oc.obs <- c(oc.obs, oc.i)
      # oc.total <- oc.total + oc.obs.n
    }
  }
  oc.obs.index <- index(data)[oc.obs]
  
  ic.obs <- which(!index(data) %in% index(data)[oc.obs])
  ic.obs.index <- index(data)[ic.obs]
  
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


###################################
## Option 1: PCA
## Single state, non-adaptive,
## not dynamic
###################################
library(mvMonitoringv2)
# Test 7 days and never update
train1A_ls <- mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic="SPE"
)
alarm.results <- train1A_ls$FaultChecks

### Classify real faults ###
library(lubridate)
n <- range(index(train.and.test.data[nrow(train.data):nrow(train.and.test.data)])[which(train.and.test.data[nrow(train.data):nrow(train.and.test.data),"MBR_1\\CURRENT_STATE"] != 1)])
real.faults <- c(interval(n[1], n[2])) # MBRs taken offline
real.faults <- c(real.faults, interval(as.POSIXct("2017-11-30 13:05:04"), as.POSIXct("2017-12-01 19:17:07"))) # when RAS TSS < 3500, see turbidity too
real.faults <- c(real.faults, interval(as.POSIXct("2017-11-29 08:36:04"), as.POSIXct("2017-11-29 08:42:04"))) # RAS Sensor fault
real.faults <- c(real.faults, interval(as.POSIXct("2017-11-29 09:53:07"), as.POSIXct("2017-11-29 16:55:22"))) # High MBR batch volume
real.faults <- c(real.faults, interval(as.POSIXct("2017-12-02 23:45:07"),as.POSIXct("2017-12-02 23:45:07"))) # RAS Sensor fault
real.faults <- c(real.faults, interval(as.POSIXct("2017-12-04 23:02:36"),as.POSIXct("2017-12-04 23:02:36"))) # RAS Sensor fault
real.faults <- c(real.faults, interval(as.POSIXct("2017-12-03 16:36:28"), as.POSIXct("2017-12-04 10:44:07"))) # BR1 Blower, MBR Batch Vol


alarm.rate.calc(alarm.results, real.faults)


###################################
## Option 2: D-PCA
## Single state, non-adaptive,
## Dynamic
###################################
library(mvMonitoringv2)
# Test 7 days and never update
train1B_ls <- mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = TRUE, ## not dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic="SPE"
)
alarm.results <- train1B_ls$FaultChecks
alarm.rate.calc(alarm.results, real.faults)

###################################
## Option 3: AD-PCA
## Single state, Adaptive,
## Dynamic
###################################
library(mvMonitoringv2)
# Test 7 days and never update
train1C_ls <- mspTrain(
  data = train.and.test.data,
  trainObs = nrow(train.data),
  labelVector = rep(1, nrow(rawData)), ## single state
  updateFreq = 1440, ## updates every day
  Dynamic = TRUE, ## dynamic
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic="SPE"
)
alarm.results <- train1C_ls$FaultChecks
alarm.rate.calc(alarm.results, real.faults)

###################################
## Option 4: AD-PCA
## Multistate, Adaptive,
## Dynamic
###################################

# Generate 'labelCol'
train.and.test.data.2 <- cbind(train.and.test.data, rep(1,nrow(train.and.test.data)))
colnames(train.and.test.data.2)[ncol(train.and.test.data.2)] <- "labelCol"

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


stateGenerator <- function(data,
                           stateVars,
                           minObs = round((2*length(which(!(colnames(data) %in% stateVars))))^2/2),
                           testingDay = NULL,
                           rollingWindowDays = 0) {
  
  # Determine the column numbers of the state variables
  colNo <- which(colnames(data) %in% stateVars)
  if(length(colNo) == 0) colNo <- which(colnames(data) %in% gsub("[\\]", ".", stateVars))
  
  # Returns character string of all state combinations
  data$labelCol <- as.numeric(do.call(paste,c(as.data.frame(data[,colNo]),sep="")))
  
  # Frequency distribution of each state?
  if (rollingWindowDays == 0) {
    freqDistribution <- as.data.frame(table(data$labelCol))
  } else {
    freqDistribution <- as.data.frame(table(data$labelCol[paste((testingDay-rollingWindowDays),"/",(testingDay-1),sep="")]))
  }
  
  # Is the frequency of each state within the subset greater than the minimum during the training period?
  statesToKeep <- as.numeric(as.character(freqDistribution[which(freqDistribution[,2] >= minObs),1]))
  
  # Test to ensure at least one state is included
  if (identical(statesToKeep, numeric(0))) {
    print("Insufficient datapoints to train. Increase rolling window size.")
    return(data)
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

train.and.test.data.2 <- stateGenerator(data = train.and.test.data.2[,colnames(train.and.test.data.2) %in% gsub("[\\]", ".", varsBR)],
                                        stateVars = stateVarsBR, 
                                        testingDay = as.Date("2017-11-29"), 
                                        rollingWindowDays = 2)


train1D_ls <- mspTrain(
  data = train.and.test.data.2[[1]][,-ncol(train.and.test.data.2[[1]])],
  trainObs = nrow(train.and.test.data.2[[1]]["/2017-11-28 23:59:47"]),
  labelVector = rep(1, nrow(train.and.test.data.2[[1]])), ## single state
  updateFreq = 1e10, 
  Dynamic = FALSE, 
  # lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 3,
  statistic="SPE"
)
alarm.results <- train1C_ls$FaultChecks
alarm.rate.calc(alarm.results, real.faults)





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

