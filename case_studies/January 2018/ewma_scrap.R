### Preliminaries ###
setwd("C:/Users/Kate Newhart/Dropbox/Newhart ADPCA Paper/R")
dataLocation <- "C:/Users/Kate Newhart/odrive/Mines/Data/MP_SBMBR_data/"
{
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
}

### Case Study Parameters ###
setwd("case_studies/January 2018")
dayOne <- as.Date("2017-12-29")
rollingWindowDays <- c(3,5,7)
daysToTest <- 23

### Classify real faults ###
library(lubridate)
real.faults <- interval(as.POSIXct("2018-01-05 09:11:13", tz="MST"), as.POSIXct("2018-01-05 11:18:17", tz="MST")) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-11 11:55:11", tz="MST"), as.POSIXct("2018-01-11 12:46", tz="MST"))) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-18 10:28:58", tz="MST"), as.POSIXct("2018-01-19 12:20:00", tz="MST")))  # BR Overdose/Relaxation mode
# Alarms from 1/22-1/25 not considered faults because not true out of control conditions, rather not necessarily "normal"


### Function: Loop through each approach ###
library(mvMonitoringv2)
library(ADPCA)
# Compile data
rawData <- loadandcleanDBF(dataLocation, dayOne+daysToTest, daysToTest)
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",dayOne+daysToTest,sep="")]
train.and.test.data <- rawData
train.and.test.data <- uniquenessCheck(train.and.test.data, N=7)
# # Plot data
# for(i in 1:length(real.faults)){
#   graph_rawData(data=train.and.test.data[paste(as.Date(real.faults[i]@start))], fault.interval=real.faults[i],keyword=paste0("fault",i))
# }
library(qcc)
ewma(data=train.and.test.data[,1], sizes=1440*rollingWindowDays[1], center=mean(train.and.test.data[,1]), std.dev)
