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


### Classify real faults ###
library(lubridate)
real.faults <- interval(as.POSIXct("2018-01-05 09:11", tz="MST"), as.POSIXct("2018-01-05 11:18", tz="MST")) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-11 11:55", tz="MST"), as.POSIXct("2018-01-11 13:55", tz="MST"))) #Sensor faults
real.faults <- c(real.faults, 
                 interval(as.POSIXct("2018-01-18 10:28", tz="MST"), as.POSIXct("2018-01-19 12:20", tz="MST")))  # BR Overdose/Relaxation mode
# Alarms from 1/22-1/25 not considered faults because not true out of control conditions, rather not necessarily "normal"

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


### Case Study Parameters ###
setwd("case_studies/July 2018")

dayOne <- as.Date("2018-08-18")
rollingWindowDays <- c(3,5,7)
daysToTest <- 30

### Function: Loop through each approach ###
library(mvMonitoringv2)
library(ADPCA)
# Compile data
rawData <- loadandcleanDBF(dataLocation, dayOne+daysToTest, daysToTest)
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",dayOne+daysToTest,sep="")]
train.and.test.data <- rawData
train.and.test.data <- uniquenessCheck(train.and.test.data, N=7)
graph_rawData(train.and.test.data[paste0(dayOne,"/",dayOne+rollingWindowDays[length(rollingWindowDays)])],"training")

graph_rawData(train.and.test.data[paste0(dayOne+rollingWindowDays[length(rollingWindowDays)],"/")],"testing")




# Plot data
for(i in 1:length(real.faults)){
  graph_rawData(data=train.and.test.data[paste(as.Date(real.faults[i]@start))], fault.interval=real.faults[i],keyword=paste0("fault",i))
}

sum(sapply(real.faults, function(x) length(which(index(train.and.test.data) %within% x))))



##### SINGLE STATE #####
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


library(doParallel)
library(foreach)
numCores <- detectCores()
registerDoParallel(numCores)
foreach.results <- foreach(training.days=rollingWindowDays,
                           .combine = 'c',
                           # .multicombine=TRUE,
                           # .init=list(list(), list()),
                           .packages = c("mvMonitoringv2",
                                         "ADPCA")) %dopar% {
                                           
                                           results.all <- list() # Store results of each training window
                                           
                                           for(r.method in 1:length(method.parameters)) { # for each method configuration
                                             # Test SPE and T2
                                             testing.stat <- "SPE"
                                             data <- train.and.test.data[,as.numeric(unlist(method.parameters[[r.method]][2]))]
                                             data <- cbind(data, rep(1, nrow(data)))
                                             lets.try.something <- c(ncol(data))
                                             error.index <- "EIGEN ERROR"
                                             while(error.index == "EIGEN ERROR") {
                                               try(expr = {
                                                 train1A_ls <- mvMonitoringv2::mspTrain(
                                                   data = data[,-lets.try.something],
                                                   trainObs = nrow(data[paste0("/",dayOne+training.days-1)]),
                                                   labelVector = rep(1, nrow(data)), 
                                                   updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
                                                   Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
                                                   faultsToTriggerAlarm = 3,
                                                   statistic=testing.stat
                                                 )
                                                 error.index <- "FINISHED"
                                               }, silent=FALSE)
                                               if(error.index == "EIGEN ERROR") {
                                                 remove.index <- min(apply(data[,-lets.try.something],2,function(x) length(unique(x))));
                                                 lets.try.something <- c(lets.try.something, which(apply(data,2,function(x) length(unique(x))) <= remove.index))
                                               }
                                             }
                                             alarm.rate.results <- as.data.frame(alarm.rate.calc(train1A_ls$FaultChecks, real.faults))
                                             alarm.results <- as.data.frame(train1A_ls$FaultChecks[,grep(testing.stat, colnames(train1A_ls$FaultChecks))])
                                             
                                             testing.stat <- "T2"
                                             lets.try.something <- c(ncol(data))
                                             error.index <- "EIGEN ERROR"
                                             while(error.index == "EIGEN ERROR") {
                                               try(expr = {
                                                 train1A_ls <- mvMonitoringv2::mspTrain(
                                                   data = data[,-lets.try.something],
                                                   trainObs = nrow(data[paste0("/",dayOne+training.days-1)]),
                                                   labelVector = rep(1, nrow(data)), 
                                                   updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
                                                   Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
                                                   faultsToTriggerAlarm = 3,
                                                   statistic=testing.stat
                                                 )
                                                 error.index <- "FINISHED"
                                               }, silent=FALSE)
                                               if(error.index == "EIGEN ERROR") {
                                                 remove.index <- min(apply(data[,-lets.try.something],2,function(x) length(unique(x))));
                                                 lets.try.something <- c(lets.try.something, which(apply(data,2,function(x) length(unique(x))) <= remove.index))
                                               }
                                             }
                                             
                                             alarm.rate.results <- cbind(alarm.rate.results, as.data.frame(alarm.rate.calc(train1A_ls$FaultChecks, real.faults)))
                                             colnames(alarm.rate.results) <- c("SPE", "T2")
                                             alarm.results <- cbind(alarm.results, as.data.frame(train1A_ls$FaultChecks[,grep(testing.stat, colnames(train1A_ls$FaultChecks))]))
                                             
                                             results.all[[length(results.all)+1]] <- list(alarm.results, alarm.rate.results)
                                             names(results.all[[length(results.all)]]) <- c("Test Results", "Alarm Statistics")
                                             names(results.all[[length(results.all)]][[2]])[1:2] <- c("SPE", "T2")
                                             names(results.all)[r.method] <- paste(as.character(unlist(method.parameters[[r.method]][1])))
                                           }
                                           
                                           return(list(results.all))
                                         }
names(foreach.results) <- paste(as.character(rollingWindowDays),"days")

save(foreach.results, file="SS_foreach_results.RData")

## Compile results
load(file="SS_foreach_results.RData")
library(xlsx)
if(exists("alarm.stats")) rm(alarm.stats)
for(x in 1:length(foreach.results)) { # For each day
  for(y in 1:length(foreach.results[[x]])) { # For each method
    
    pull.data <- foreach.results[[x]][[y]][[2]]
    pull.data[c(1:3), 1] <- pull.data[c(1:3), 2]
    
    if(!exists("alarm.stats")) {
      alarm.stats <- as.data.frame(pull.data[,1], row.names = rownames(pull.data))
      colnames(alarm.stats) <- paste(names(foreach.results[[x]])[y], names(foreach.results)[x])
    } else {
      alarm.stats <- cbind(alarm.stats, alarm.stats <- as.data.frame(pull.data[,1], row.names = rownames(pull.data)))
      colnames(alarm.stats)[ncol(alarm.stats)] <- paste(names(foreach.results[[x]])[y], names(foreach.results)[x])
    }
  }
}
write.xlsx(alarm.stats, file="SS_results_all.xlsx")

for(x in 1:length(foreach.results)) { # For each day
  for(y in 1:length(foreach.results[[x]])) { # For each method
    pull.data <- foreach.results[[x]][[y]][[1]]
    # write.xlsx(pull.data, file="SS_results_all.xlsx", sheetName=paste(names(foreach.results[[x]])[y], names(foreach.results)[x]), append=TRUE, row.names=TRUE)
    write.csv(pull.data, file=paste(names(foreach.results[[x]])[y], names(foreach.results)[x],".csv"))
    
  }
}


## PLOT RESULTS
load(file="SS_foreach_results.RData")
alarm.results <- foreach(i=1:length(foreach.results),
                         .combine = 'c') %dopar% { # For each day
                           
                           for(j in 1:length(foreach.results[[i]])) { # For each method
                             foreach.results[[i]][[j]][[1]] <- cbind(foreach.results[[i]][[j]][[1]], rep(NA, nrow(foreach.results[[i]][[j]][[1]])))
                             colnames(foreach.results[[i]][[j]][[1]])[ncol(foreach.results[[i]][[j]][[1]])] <-"Alarm"
                             for(k in 1:nrow(foreach.results[[i]][[j]][[1]])) {
                               if((foreach.results[[i]][[j]][[1]][k,2] == 0) && (foreach.results[[i]][[j]][[1]][k,4] == 0)) foreach.results[[i]][[j]][[1]][k,5] <- 0
                               if((foreach.results[[i]][[j]][[1]][k,2] == 0) && (foreach.results[[i]][[j]][[1]][k,4] == 1)) foreach.results[[i]][[j]][[1]][k,5] <- 1
                               if((foreach.results[[i]][[j]][[1]][k,2] == 1) && (foreach.results[[i]][[j]][[1]][k,4] == 0)) foreach.results[[i]][[j]][[1]][k,5] <- 2
                               if((foreach.results[[i]][[j]][[1]][k,2] == 1) && (foreach.results[[i]][[j]][[1]][k,4] == 1)) foreach.results[[i]][[j]][[1]][k,5] <- 3
                             }
                             # results <- as.data.frame(alarm.rate.calc(FaultChecks = xts(foreach.results[[i]][[j]][[1]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]][[1]]))), real.alarms = real.faults))
                             # colnames(results) <- paste(names(foreach.results[[i]])[j])
                             results <- as.data.frame(foreach.results[[i]][[j]][[1]])
                             
                             if(j==1) results.all <- list(results)
                             if(j > 1) results.all <- c(results.all, list(results))
                             
                             names(results.all)[length(names(results.all))] <- paste(names(foreach.results[[i]])[j])
                           }
                           results.list <- list(results.all)
                           names(results.list) <- paste(names(foreach.results)[i])
                           return(results.list)
                         }

for(i in 1:length(foreach.results)) { # For each day
  for(j in 1:length(foreach.results[[x]])) { # For each method
plot.index <- range(as.POSIXct(rownames(foreach.results[[i]][[j]][[1]])))
plot.data <- cbind(rawData[paste0(plot.index[1],"/",plot.index[2])], xts(foreach.results[[i]][[j]][[1]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]][[1]]))))
graph_alarmData(data=plot.data, keyword = paste0("plots//",names(foreach.results[[i]])[j]," ",names(foreach.results)[i]), fault.interval = real.faults, filetype = "png")

  }
}


##### MULTISTATE #####
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


# Train and test each rolling window
# for(training.days in rollingWindowDays) {
library(doParallel)
library(foreach)
numCores <- detectCores()
registerDoParallel(numCores)
foreach.results <- foreach(training.days=rollingWindowDays,
                           .combine = 'c',
                           # .multicombine=TRUE,
                           # .init=list(list(), list()),
                           .packages = c("mvMonitoringv2",
                                         "ADPCA")) %dopar% {
  
  train.and.test.data.br <- stateGenerator(data = train.and.test.data.2,
                                             stateVars = stateVarsBR, 
                                             vars=varsBR,
                                             rollingWindowDays = training.days)

  train.and.test.data.mt <- stateGenerator(data = train.and.test.data.2,
                                             stateVars = stateVarsMT, 
                                             vars=varsMT,
                                             rollingWindowDays = training.days)

  for(r.method in 1:length(method.parameters)) {
    
    if(length(grep("BR", method.parameters[[r.method]][1]))==1) {
      train.and.test.data.loop <- train.and.test.data.br
    } else {
      train.and.test.data.loop <- train.and.test.data.mt
    }
    results.all <- vector()
    
    for(i in 1:length(train.and.test.data.loop)) { # For each state
  
      testing.stat <- "SPE"
      data <- train.and.test.data.loop[[i]]
      lets.try.something <- c(ncol(train.and.test.data.loop[[i]]))
      error.index <- "EIGEN ERROR"
      while(error.index == "EIGEN ERROR") {
        try(expr = {
          train1A_ls <- mspTrain(
            data = data[,-lets.try.something],
            trainObs = nrow(data[paste0("/",dayOne+training.days-1)]),
            labelVector = rep(1, nrow(data)), 
            updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
            Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
            faultsToTriggerAlarm = 3,
            statistic=testing.stat
          )
          error.index <- "FINISHED"
        }, silent=FALSE)
        if(error.index == "EIGEN ERROR") {
          remove.index <- min(apply(data[,-lets.try.something],2,function(x) length(unique(x))));
          lets.try.something <- c(lets.try.something, which(apply(data,2,function(x) length(unique(x))) <= remove.index))
        }
      }
      alarm.results <- as.data.frame(train1A_ls$FaultChecks[,grep(testing.stat, colnames(train1A_ls$FaultChecks))])
      
      testing.stat <- "T2"
      lets.try.something <- c(ncol(train.and.test.data.loop[[i]]))
      error.index <- "EIGEN ERROR"
      while(error.index == "EIGEN ERROR") {
        try(expr = {
          train1A_ls <- mspTrain(
            data = data[,-lets.try.something],
            trainObs = nrow(data[paste0("/",dayOne+training.days-1)]),
            labelVector = rep(1, nrow(data)), 
            updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
            Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
            faultsToTriggerAlarm = 3,
            statistic=testing.stat
          )
          error.index <- "FINISHED"
        }, silent=FALSE)
        if(error.index == "EIGEN ERROR") {
          remove.index <- min(apply(data[,-lets.try.something],2,function(x) length(unique(x))));
          lets.try.something <- c(lets.try.something, which(apply(data,2,function(x) length(unique(x))) <= remove.index))
        }
      }
      alarm.results <- cbind(alarm.results, as.data.frame(train1A_ls$FaultChecks[,grep(testing.stat, colnames(train1A_ls$FaultChecks))]))
      if(i == 1) alarm.results.state <- alarm.results
      if(i > 1) alarm.results.state <- rbind(alarm.results.state, alarm.results)
    }

    if(r.method==1) alarm.results.method <- list(alarm.results.state)
    if(r.method>1) alarm.results.method <- c(alarm.results.method, list(alarm.results.state))
    names(alarm.results.method)[[length(alarm.results.method)]] <- paste(as.character(unlist(method.parameters[[r.method]][1])))
  }
  results <- list(alarm.results.method)
  names(results) <- paste(training.days,"days")
  return(results)
}
save(foreach.results, file="MS_foreach_results")

alarm.results <- foreach(i=1:length(foreach.results),
                         .combine = 'c') %dopar% { # For each day

  for(j in 1:length(foreach.results[[i]])) { # For each method
    foreach.results[[i]][[j]] <- cbind(foreach.results[[i]][[j]], rep(NA, nrow(foreach.results[[i]][[j]])))
    colnames(foreach.results[[i]][[j]])[ncol(foreach.results[[i]][[j]])] <-"Alarm"
    for(k in 1:nrow(foreach.results[[i]][[j]])) {
      if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 0
      if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 1
      if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 2
      if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 3
    }
    results <- as.data.frame(alarm.rate.calc(FaultChecks = xts(foreach.results[[i]][[j]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]]))), real.alarms = real.faults))
    colnames(results) <- paste(names(foreach.results[[i]])[j])
    
    if(j==1) results.all <- results
    if(j > 1) results.all <- cbind(results.all, results)
  }
  results.list <- list(results.all)
  names(results.list) <- paste(names(foreach.results)[i])
  return(results.list)
}


for(x in 1:length(alarm.results)) { # For each day
  pull.data <- as.data.frame(alarm.results[[x]])
  colnames(pull.data) <- paste(colnames(pull.data), names(alarm.results)[x])
  if(x==1) results.cbind <- pull.data
  if(x>1) results.cbind <- cbind(results.cbind, pull.data)
}
write.xlsx(results.cbind, file="MS_results_all.xlsx")

for(x in 1:length(foreach.results)) { # For each day
  for(y in 1:length(foreach.results[[x]])) { # For each method
    pull.data <- foreach.results[[x]][[y]]
    # write.xlsx(pull.data, file="MS_results_all.xlsx", sheetName=paste(names(foreach.results[[x]])[y], names(foreach.results)[x]), append=TRUE, row.names=TRUE)
    write.csv(pull.data, file=paste(names(foreach.results[[x]])[y], names(foreach.results)[x],".csv"))
    # print(nrow(pull.data))
  }
}




load(file="MS_foreach_results.RData")
alarm.results <- foreach(i=1:length(foreach.results),
                         .combine = 'c') %dopar% { # For each day
                           
                           for(j in 1:length(foreach.results[[i]])) { # For each method
                             foreach.results[[i]][[j]] <- cbind(foreach.results[[i]][[j]], rep(NA, nrow(foreach.results[[i]][[j]])))
                             colnames(foreach.results[[i]][[j]])[ncol(foreach.results[[i]][[j]])] <-"Alarm"
                             for(k in 1:nrow(foreach.results[[i]][[j]])) {
                               if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 0
                               if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 1
                               if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 2
                               if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 3
                             }
        
                             results <- as.data.frame(foreach.results[[i]][[j]])
                             
                             if(j==1) results.all <- list(results)
                             if(j > 1) results.all <- c(results.all, list(results))
                             
                             names(results.all)[length(names(results.all))] <- paste(names(foreach.results[[i]])[j])
                           }
                           results.list <- list(results.all)
                           names(results.list) <- paste(names(foreach.results)[i])
                           return(results.list)
                         }

for(i in 1:length(foreach.results)) { # For each day
  for(j in 1:length(foreach.results[[x]])) { # For each method
    plot.index <- range(as.POSIXct(rownames(foreach.results[[i]][[j]])))
    plot.data <- cbind(rawData[paste0(plot.index[1],"/",plot.index[2])], xts(foreach.results[[i]][[j]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]]))))
    graph_alarmData(data=plot.data, keyword = paste0("plots//",names(foreach.results[[i]])[j]," ",names(foreach.results)[i]), fault.interval = real.faults, filetype = "png")
  }
}





##### ETC. ####



# ### MEWMA TAKES INFINITY AND BEYOND, RUN OVERNIGHT?
# library(MSQC) ## for MEWMA and MCUSUM
# train <- train.and.test.data[paste0(dayOne,"/",dayOne+training.days)]
# train <- train[,which(as.numeric(apply(train,2,function(x) length(unique(x)))) > 20)]
# train <- as.matrix(train[1:100,])
# Xmv <- mult.chart(x=train, type = "t2")
