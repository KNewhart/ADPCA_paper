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
setwd("case_studies/March 2018")
dayOne <- as.Date("2018-02-24")
rollingWindowDays <- c(3,5,7)
daysToTest <- 14

# ### Classify real faults ###
library(lubridate)
real.faults <- interval(as.POSIXct("2018-03-08 08:42", tz="MST"), as.POSIXct("2018-03-08 14:13", tz="MST"))  # Bio 2 level
real.faults <- c(real.faults,
                 interval(as.POSIXct("2018-03-10 05:00", tz="MST"), as.POSIXct("2018-03-11 00:00", tz="MST"))) # Bio 1 level, Bio 1 TSS


### Function: Loop through each approach ###
library(mvMonitoringv2)
library(ADPCA)
# Compile data
rawData <- loadandcleanDBF(dataLocation, dayOne+daysToTest, daysToTest)
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",dayOne+daysToTest,sep="")]
train.and.test.data <- rawData
graph_rawData(rawData[paste0(dayOne,"/",dayOne+7)],"training")
graph_rawData(rawData, "training_and_testing", fault.interval = real.faults)


train.and.test.data <- uniquenessCheck(train.and.test.data, N=7)
# Plot data
for(i in 1:length(real.faults)){
  graph_rawData(data=train.and.test.data[paste(as.Date(real.faults[i]@start))], fault.interval=real.faults[i],keyword=paste0("fault",i))
}

# Define methods
method.parameters <- list()
method.parameters[[length(method.parameters)+1]] <- list(method="SS PCA", 
                                                         dataCols=seq(1,ncol(train.and.test.data)),
                                                         updateFreq = 1e10,
                                                         Dynamic=FALSE
)
method.parameters[[length(method.parameters)+1]] <-
  list(method="BR SS PCA",
       dataCols=which(colnames(train.and.test.data) %in% varsBR),
       updateFreq = 1e10,
       Dynamic=FALSE
  )
method.parameters[[length(method.parameters)+1]] <- 
  list(method="MT SS PCA",
       dataCols=which(colnames(train.and.test.data) %in% varsMT),
       updateFreq = 1e10,
       Dynamic=FALSE
  )
method.parameters[[length(method.parameters)+1]] <- list(method="SS D-PCA", 
                                                         dataCols=seq(1,ncol(train.and.test.data)),
                                                         updateFreq = 1e10,
                                                         Dynamic=TRUE
)
method.parameters[[length(method.parameters)+1]] <-
  list(method="BR SS D-PCA",
       dataCols=which(colnames(train.and.test.data) %in% varsBR),
       updateFreq = 1e10,
       Dynamic=TRUE
  )
method.parameters[[length(method.parameters)+1]] <- 
  list(method="MT SS D-PCA",
       dataCols=which(colnames(train.and.test.data) %in% varsMT),
       updateFreq = 1e10,
       Dynamic=TRUE
  )
method.parameters[[length(method.parameters)+1]] <- list(method="SS AD-PCA", 
                                                         dataCols=seq(1,ncol(train.and.test.data)),
                                                         updateFreq = 1440,
                                                         Dynamic=TRUE
)
method.parameters[[length(method.parameters)+1]] <-
  list(method="BR SS AD-PCA",
       dataCols=which(colnames(train.and.test.data) %in% varsBR),
       updateFreq = 1440,
       Dynamic=TRUE
  )
method.parameters[[length(method.parameters)+1]] <- 
  list(method="MT SS AD-PCA",
       dataCols=which(colnames(train.and.test.data) %in% varsMT),
       updateFreq = 1440,
       Dynamic=TRUE
  )



# Train and test each rolling window
compiled.results.all <- array(data=NA, c(6,length(method.parameters),length(rollingWindowDays)))
colnames.compiled.results.all <- list()
# Calculate fault detection accuracy
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
  if(is.nan(tdr.t2)) tdr.t2 <- 0
  
  true.detection.spe <- which(alarmed.spe.obs %in% oc.obs)
  tdr.spe <- length(true.detection.spe)/length(oc.obs)
  if(is.nan(tdr.spe)) tdr.spe <- 0
  
  # IC obs are alarmed "false alarms"
  alarmed.t2.ic.obs <- which(alarmed.t2.obs %in% ic.obs)
  fdr.t2 <- length(alarmed.t2.ic.obs)/length(ic.obs)
  if(is.nan(fdr.t2)) fdr.t2 <- 0
  
  alarmed.spe.ic.obs <- which(alarmed.spe.obs %in% ic.obs)
  fdr.spe <- length(alarmed.spe.ic.obs)/length(ic.obs)
  if(is.nan(fdr.spe)) fdr.spe <- 0
  
  # OC observations are not alarmed "missed alarms"
  nonalarmed.t2.oc.obs <- which(nonalarmed.t2.obs %in% oc.obs)
  ficr.t2 <- length(nonalarmed.t2.oc.obs)/length(oc.obs)
  if(is.nan(ficr.t2)) ficr.t2 <- 0
  
  nonalarmed.spe.oc.obs <- which(nonalarmed.spe.obs %in% oc.obs)
  ficr.spe <- length(nonalarmed.spe.oc.obs)/length(oc.obs)
  if(is.nan(ficr.spe)) ficr.spe <- 0
  
  return(c("TDR-T2" = tdr.t2,
           "FDR-T2" = fdr.t2, 
           "FICR-T2" = ficr.t2,
           "TDR-SPE" = tdr.spe,
           "FDR-SPE" = fdr.spe, 
           "FICR-SPE" = ficr.spe))
}


# library(doParallel)
# library(foreach)
# numCores <- detectCores()
# registerDoParallel(numCores)
# foreach(training.days=rollingWindowDays) %dopar% {
#   library(mvMonitoringv2)
#   library(ADPCA)
  
for(training.days in rollingWindowDays) {
  # Train and test each method
  print(paste("Starting", training.days, "day rolling window"))
  results.all <- vector()
  r.method <- 1
  for(r.method in 1:length(method.parameters)) {
    testing.stat <- "SPE"
    lets.try.something <- c(ncol(train.and.test.data))
    error.index <- "EIGEN ERROR"
    while(error.index == "EIGEN ERROR") {
      try(expr = {
        train1A_ls <- mvMonitoringv2::mspTrain(
          data = train.and.test.data[,-lets.try.something],
          trainObs = nrow(train.and.test.data[paste0("/",dayOne+training.days)]),
          labelVector = rep(1, nrow(train.and.test.data)), 
          updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
          Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
          faultsToTriggerAlarm = 3,
          statistic=testing.stat
        )
        error.index <- "FINISHED"
      }, silent=FALSE)
      if(error.index == "EIGEN ERROR") {
        remove.index <- min(apply(train.and.test.data[,-lets.try.something],2,function(x) length(unique(x))));
        lets.try.something <- c(lets.try.something, which(apply(train.and.test.data,2,function(x) length(unique(x))) <= remove.index))
      }
    }
    alarm.results <- as.data.frame(alarm.rate.calc(train1A_ls$FaultChecks, real.faults))
    print(paste(as.character(unlist(method.parameters[[r.method]][1])), testing.stat,"completed"))
    
    testing.stat <- "T2"
    lets.try.something <- c(ncol(train.and.test.data))
    error.index <- "EIGEN ERROR"
    while(error.index == "EIGEN ERROR") {
      try(expr = {
        train1A_ls <- mvMonitoringv2::mspTrain(
          data = train.and.test.data[,-lets.try.something],
          trainObs = nrow(train.and.test.data[paste0("/",dayOne+training.days)]),
          labelVector = rep(1, nrow(train.and.test.data)), 
          updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
          Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
          faultsToTriggerAlarm = 3,
          statistic=testing.stat
        )
        error.index <- "FINISHED"
      }, silent=FALSE)
      if(error.index == "EIGEN ERROR") {
        remove.index <- min(apply(train.and.test.data[,-lets.try.something],2,function(x) length(unique(x))));
        lets.try.something <- c(lets.try.something, which(apply(train.and.test.data,2,function(x) length(unique(x))) <= remove.index))
      }
    }
    alarm.results <- cbind(alarm.results, as.data.frame(alarm.rate.calc(train1A_ls$FaultChecks, real.faults)))
    alarm.results[c(1:3),1] <- alarm.results[c(1:3),2]
    results.all <- cbind(results.all, alarm.results[,1])
    colnames(results.all)[r.method] <- as.character(unlist(method.parameters[[r.method]][1]))
    print(paste(as.character(unlist(method.parameters[[r.method]][1])), testing.stat,"completed"))
  }
  compiled.results.all[,,which(training.days == rollingWindowDays)] <- results.all
  colnames.compiled.results.all[[which(training.days == rollingWindowDays)]] <- colnames(results.all)
}

ss_results <- compiled.results.all
ss_cols <- colnames.compiled.results.all
# 
# # Save results
# library(xlsx)
# for(i in 1:length(rollingWindowDays)) {
# 
#     new.data <- as.data.frame(compiled.results.all[,,i])
#     colnames(new.data) <- colnames.compiled.results.all[[i]]
#     rownames(new.data) <- rownames(alarm.results)
#     save.data <- as.data.frame(cbind(rep(rollingWindowDays[i],6), new.data))
#     if(i == 1) write.xlsx(save.data, file="SS_results.xlsx", sheetName=paste(rollingWindowDays[i],"days"), TRUE)
#     if(i > 1) write.xlsx(save.data, file="SS_results.xlsx", append=TRUE, sheetName=paste(rollingWindowDays[i],"days"), row.names=TRUE)
# }



### ADD MULTISTATE ###
train.and.test.data.2 <- rawData
method.parameters <- list()
method.parameters[[length(method.parameters)+1]] <- list(method="MS BR PCA", 
                                                         dataCols=which(colnames(train.and.test.data.2) %in% varsBR),
                                                         updateFreq = 1e10,
                                                         Dynamic=FALSE)
method.parameters[[length(method.parameters)+1]] <- list(method="MS MT PCA", 
                                                         dataCols=which(colnames(train.and.test.data.2) %in% varsMT),
                                                         updateFreq = 1e10,
                                                         Dynamic=FALSE)

method.parameters[[length(method.parameters)+1]] <- list(method="MS BR D-PCA", 
                                                         dataCols=which(colnames(train.and.test.data.2) %in% varsBR),
                                                         updateFreq = 1e10,
                                                         Dynamic=TRUE)
method.parameters[[length(method.parameters)+1]] <- list(method="MS MT D-PCA", 
                                                         dataCols=which(colnames(train.and.test.data.2) %in% varsMT),
                                                         updateFreq = 1e10,
                                                         Dynamic=TRUE)

method.parameters[[length(method.parameters)+1]] <- list(method="MS BR AD-PCA", 
                                                         dataCols=which(colnames(train.and.test.data.2) %in% varsBR),
                                                         updateFreq = 1440,
                                                         Dynamic=TRUE)
method.parameters[[length(method.parameters)+1]] <-
  list(method="MS MT AD-PCA",
       dataCols=which(colnames(train.and.test.data.2) %in% varsMT),
       updateFreq = 1440,
       Dynamic=TRUE)
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

# train.and.test.data.2 <- uniquenessCheck(train.and.test.data.2, N=7)
compiled.results.all <- array(data=NA, c(6,length(method.parameters),length(rollingWindowDays)))
colnames.compiled.results.all <- list()

# Train and test each rolling window
for(training.days in rollingWindowDays) {
  
  print(paste("Starting", training.days, "day rolling window"))
  
  train.and.test.data.br <- stateGenerator(data = train.and.test.data.2,
                                           stateVars = stateVarsBR, 
                                           vars=varsBR,
                                           rollingWindowDays = training.days)
  
  train.and.test.data.mt <- stateGenerator(data = train.and.test.data.2,
                                           stateVars = stateVarsMT, 
                                           vars=varsMT,
                                           rollingWindowDays = training.days)
  results.all.sum <- vector()
  for(r.method in 1:length(method.parameters)) {
    
    if(length(grep("BR", method.parameters[[r.method]][1]))==1) {
      train.and.test.data.loop <- train.and.test.data.br
    } else {
      train.and.test.data.loop <- train.and.test.data.mt
    }
    results.all <- vector()
    for(i in 1:length(train.and.test.data.loop)) {
      
      
      
      testing.stat <- "SPE"
      lets.try.something <- c(ncol(train.and.test.data.loop[[i]]))
      error.index <- "EIGEN ERROR"
      while(error.index == "EIGEN ERROR") {
        try(expr = {
          train1A_ls <- mvMonitoringv2::mspTrain(
            data = train.and.test.data.loop[[i]][,-lets.try.something],
            trainObs = nrow(train.and.test.data.loop[[i]][paste0("/",dayOne+training.days)]),
            labelVector = rep(1, nrow(train.and.test.data.loop[[i]])), 
            updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
            Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
            faultsToTriggerAlarm = 3,
            statistic=testing.stat
          )
          error.index <- "FINISHED"
        }, silent=FALSE)
        if(error.index == "EIGEN ERROR") {
          remove.index <- min(apply(train.and.test.data.loop[[i]][,-lets.try.something],2,function(x) length(unique(x))));
          lets.try.something <- c(lets.try.something, which(apply(train.and.test.data.loop[[i]],2,function(x) length(unique(x))) <= remove.index))
        }
      }
      alarm.results <- as.data.frame(alarm.rate.calc(FaultChecks=train1A_ls$FaultChecks, real.alarms=real.faults))
      print(paste(as.character(unlist(method.parameters[[r.method]][1])), testing.stat,"completed"))
      
      testing.stat <- "T2"
      lets.try.something <- c(ncol(train.and.test.data.loop[[i]]))
      error.index <- "EIGEN ERROR"
      while(error.index == "EIGEN ERROR") {
        try(expr = {
          train1A_ls <- mvMonitoringv2::mspTrain(
            data = train.and.test.data.loop[[i]][,-lets.try.something],
            trainObs = nrow(train.and.test.data.loop[[i]][paste0("/",dayOne+training.days)]),
            labelVector = rep(1, nrow(train.and.test.data.loop[[i]])), 
            updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
            Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
            faultsToTriggerAlarm = 3,
            statistic=testing.stat
          )
          error.index <- "FINISHED"
        }, silent=FALSE)
        if(error.index == "EIGEN ERROR") {
          remove.index <- min(apply(train.and.test.data.loop[[i]][,-lets.try.something],2,function(x) length(unique(x))));
          lets.try.something <- c(lets.try.something, which(apply(train.and.test.data.loop[[i]],2,function(x) length(unique(x))) <= remove.index))
        }
      }
      
      alarm.results <- cbind(alarm.results, as.data.frame(alarm.rate.calc(train1A_ls$FaultChecks, real.faults)))
      alarm.results[c(1:3),1] <- alarm.results[c(1:3),2]
      alarm.results <-alarm.results[,1]
      results.all <- cbind(results.all, alarm.results)
    }
    # colnames(results.all)[r.method] <- as.character(unlist(method.parameters[[r.method]][1]))
    print(paste(as.character(unlist(method.parameters[[r.method]][1])), testing.stat,"completed"))
    
    n.testing.obs <- unlist(lapply(train.and.test.data.loop, function(x) nrow(x[paste0("/",dayOne+training.days)])))
    summary.results <- matrix(NA, nrow=nrow(results.all), ncol=length(n.testing.obs))
    # Multiply each experimental result by the population frequency to weigh the results of each experiment
    for(j in 1:length(n.testing.obs)) {
      summary.results[,j] <- results.all[,j]*n.testing.obs[j]/sum(n.testing.obs)
    }
    alarm.results.sum <- apply(summary.results,1,sum)
    results.all.sum <- cbind(results.all.sum, alarm.results.sum)
    colnames(results.all.sum)[ncol(results.all.sum)] <- as.character(unlist(method.parameters[[r.method]][1]))
  }
  
  compiled.results.all[,,which(training.days == rollingWindowDays)] <- results.all.sum
  # colnames.compiled.results.all[[which(training.days == rollingWindowDays)]] <- colnames(results.all)
}


ms_results <- compiled.results.all
ms_cols <- colnames.compiled.results.all

# Save results
library(xlsx)
for(i in 1:length(rollingWindowDays)) {
  
  new.data <- as.data.frame(ss_results[,,i])
  colnames(new.data) <- ss_cols[[i]]
  rownames(new.data) <- rownames(alarm.results)
  new.data <- cbind(new.data, as.data.frame(ms_results[,,i]))
  # colnames(new.data)[(ncol(new.data)-1):ncol(new.data)] <- ms_cols[[i]]
  save.data <- as.data.frame(cbind(rep(rollingWindowDays[i],6), new.data))
  if(i == 1) write.xlsx(save.data, file="MS_SS_results.xlsx", sheetName=paste(rollingWindowDays[i],"days"), TRUE)
  if(i > 1) write.xlsx(save.data, file="MS_SS_results.xlsx", append=TRUE, sheetName=paste(rollingWindowDays[i],"days"), row.names=TRUE)
}





