### Preliminaries ###
library(mvMonitoringv2)
library(ADPCA)
library(xts)

source("src/constants.R")

# Starting day
dayOne <- dayN - 2*rollingWindowDays

# Range of dates to compile
daysToTest <- as.numeric(difftime(dayN, dayOne)) + 1

# Move into folder to save results
setwd(paste0("results/",folder))


### Run analysis
foreach.results <- list()
for(i in 1:length(rollingWindowDays)) {

  # Compile data
  rawData <- loadandcleanDBF(dataLocation, dayOne[i]+daysToTest[i], daysToTest[i])
  rawData <- xts(rawData[,-1], order.by = rawData[,1])
  rawData <- rawData[paste("/",dayOne[i]+daysToTest[i],sep="")]
  train.and.test.data <- rawData
  
  train.and.test.data <- uniquenessCheck(train.and.test.data, N=rollingWindowDays[i])
  graph_rawData(train.and.test.data,paste0("plots/",paste("training", rollingWindowDays[i])))
  
  
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
  
  
  
  results.all <- list() # Store results of each training window
  
  # for(r.method in 1:length(method.parameters)) { # for each method configuration
  library(doParallel)
  library(foreach)
  numCores <- detectCores()
  registerDoParallel(numCores)
  results.all <- foreach(r.method=1:length(method.parameters),
                             .combine = 'c',
                             # .multicombine=TRUE,
                             # .init=list(list(), list()),
                             .packages = c("mvMonitoring","mvMonitoringv2",
                                           "ADPCA")) %dopar% {
    # Test SPE, T2, and SPE+T2
    testing.stat <- "SPE"
    data <- train.and.test.data[,as.numeric(unlist(method.parameters[[r.method]][2]))]
    data <- cbind(data, rep(1, nrow(data)))
    lets.try.something <- c(ncol(data))
    error.index <- "EIGEN ERROR"
    while(error.index == "EIGEN ERROR") {
      try(expr = {
        train1A_ls <- mvMonitoringv2::mspTrain(
          data = data[,-lets.try.something],
          trainObs = nrow(data[paste0("/",dayOne[i]+rollingWindowDays[i]-1)]),
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
    alarm.results <- as.data.frame(merge(train1A_ls$Non_Alarmed_Obs, train1A_ls$FaultChecks))
    alarm.results <- cbind(alarm.results, rep(train1A_ls$TrainingSpecs[[1]]$SPE_threshold, nrow(alarm.results)))
    colnames(alarm.results)[ncol(alarm.results)] <- "SPE_threshold"
    results.all[[length(results.all)+1]] <- list(alarm.results)
    
    testing.stat <- "T2"
    lets.try.something <- c(ncol(data))
    error.index <- "EIGEN ERROR"
    while(error.index == "EIGEN ERROR") {
      try(expr = {
        train1A_ls <- mvMonitoringv2::mspTrain(
          data = data[,-lets.try.something],
          trainObs = nrow(data[paste0("/",dayOne[i]+rollingWindowDays[i]-1)]),
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
    alarm.results <- as.data.frame(merge(train1A_ls$Non_Alarmed_Obs, train1A_ls$FaultChecks))
    alarm.results <- cbind(alarm.results, rep(train1A_ls$TrainingSpecs[[1]]$T2_threshold, nrow(alarm.results)))
    colnames(alarm.results)[ncol(alarm.results)] <- "T2_threshold"
    results.all[[length(results.all)]] <- c(results.all[[length(results.all)]],
                                            list(alarm.results))
    
    
    
    testing.stat <- "SPE_T2"
    lets.try.something <- c(ncol(data))
    error.index <- "EIGEN ERROR"
    while(error.index == "EIGEN ERROR") {
      try(expr = {
        train1A_ls <- mvMonitoringv2::mspTrain(
          data = data[,-lets.try.something],
          trainObs = nrow(data[paste0("/",dayOne[i]+rollingWindowDays[i]-1)]),
          labelVector = rep(1, nrow(data)), 
          updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
          Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
          faultsToTriggerAlarm = 3,
          statistic <- testing.stat
        )
        error.index <- "FINISHED"
      }, silent=FALSE)
      if(error.index == "EIGEN ERROR") {
        remove.index <- min(apply(data[,-lets.try.something],2,function(x) length(unique(x))));
        lets.try.something <- c(lets.try.something, which(apply(data,2,function(x) length(unique(x))) <= remove.index))
      }
    }
    alarm.results <- as.data.frame(merge(train1A_ls$Non_Alarmed_Obs, train1A_ls$FaultChecks))
    alarm.results <- cbind(alarm.results, rep(train1A_ls$TrainingSpecs[[1]]$SPE_threshold, nrow(alarm.results)), rep(train1A_ls$TrainingSpecs[[1]]$T2_threshold, nrow(alarm.results)))
    colnames(alarm.results)[(ncol(alarm.results)-1):ncol(alarm.results)] <- c("SPE_threshold","T2_threshold")
    results.all[[length(results.all)]] <- c(results.all[[length(results.all)]],
                                            list(alarm.results))
   
    return(results.all)
  }
  foreach.results <- c(foreach.results, list(results.all))
}

names(foreach.results) <- paste(as.character(rollingWindowDays),"days")

save(foreach.results, file="SS_foreach_results.RData")


















plotTimeseries <- function(data1) {
  label1 <- colnames(data1)

  r1 <- range(index(data1)[which(!is.na(data1))])[1]
  r2 <- range(index(data1)[which(!is.na(data1))])[2]

  data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
  data2plot <- data.frame(data2plot, row.names = as.character(index(data2plot)))
  data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))

  par(mar=c(3.1,4.1,0.6,2.1))
  plot(x = data2plot[,ncol(data2plot)], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
  axis(side = 2)
  mtext(side = 2, label1, line = 2.5)

  # x-axis
  axis.ticks <- seq(0,round(data2plot[nrow(data2plot),ncol(data2plot)]), by = 10)
  if(axis.ticks == 0) axis.ticks <- seq(0,round(data2plot[nrow(data2plot),ncol(data2plot)]), by = 1/24)
  axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,ncol(data2plot)]))
  axis.labels <- sapply(axis.labels, function(x) x[length(x)])
  axis.labels[[1]] <- 1
  axis.labels <- as.numeric(unlist(axis.labels))
  if(length(unique(format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))) > 1) axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))
  if(length(unique(format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))) == 1) axis(side = 1, at = axis.ticks, labels = format(round(as.POSIXct(rownames(data2plot)[axis.labels]), "hour"), "%H"))

}

pdf(file="SS_SPE_T2_test.pdf", width=8.5, height =11)

n <- c(1,1,1,2,4,6,2,4,7,3,5,8,3,5,9,10,11,12)
m <- matrix(n, ncol=3, byrow=TRUE)
# m <- matrix(c(n,n+n[length(n)]), ncol=3, byrow = TRUE)
# layout(m,widths = c(0.33,0.33,0.33), heights = c(0.03,0.22,0.22,0.22,0.22,0.11,0.11,0.11,0.11,0.03,0.03,0.03,0.03))
layout(m, widths=c(0.33,0.33,0.33), heights=c(0.03,0.85/6,0.85/6,0.85/6,0.85/6,0.85/12,0.85/12,0.85/12,0.85/12,0.03,0.03,0.03))

## Compile results
for(x in 1:length(foreach.results)) { # For each day
  for(y in 1:length(foreach.results[[x]])) { # For each method
    pull.spe.data <- foreach.results[[x]][[y]][[1]]
    pull.spe.data <- xts(pull.spe.data, order.by=as.POSIXct(row.names(pull.spe.data)))

    pull.t2.data <- foreach.results[[x]][[y]][[2]]
    pull.t2.data <- xts(pull.t2.data, order.by = as.POSIXct(row.names(pull.t2.data)))
    
    pull.spe.t2.data <- foreach.results[[x]][[y]][[3]]
    pull.spe.t2.data <- xts(pull.spe.t2.data, order.by=as.POSIXct(row.names(pull.spe.t2.data)))

    # Text L
    par(mar=c(0,0,0,0))
    plot(0,0,type = "n",frame.plot = FALSE,axes = FALSE)
    u <- par("usr")
    text(0, u[4], labels = paste(rollingWindowDays[x],method.parameters[[y]][[1]]),
         col = "red", pos = 1, cex=2)

    plotTimeseries(pull.spe.data[paste0(dayN)][,"SPE"])
    plotTimeseries(pull.spe.data[paste0(dayN)][,"SPE_Flag"])
    u <- par("usr")
    if(length(which(pull.spe.data[paste0(dayN)][,"SPE_Flag"] != 0)) > 0) text(0, 0.9,
                                                                              labels=format(index(pull.spe.data[paste0(dayN)])[which(pull.spe.data[paste0(dayN)][,"SPE_Flag"] != 0)[1]], "%H:%M"),
                                                                              col="red", pos = 4, cex=1.5)
    plotTimeseries(pull.t2.data[paste0(dayN)][,"T2"])
    plotTimeseries(pull.t2.data[paste0(dayN)][,"T2_Flag"])
    u <- par("usr")
    if(length(which(pull.t2.data[paste0(dayN)][,"T2_Flag"] != 0)) > 0) text(0, 0.9,
                                                                             labels=format(index(pull.t2.data[paste0(dayN)])[which(pull.t2.data[paste0(dayN)][,"T2_Flag"] != 0)[1]], "%H:%M"),
                                                                             col="red", pos = 4, cex=1.5)
    
    plotTimeseries(pull.spe.t2.data[paste0(dayN)][,"SPE"])
    plotTimeseries(pull.spe.t2.data[paste0(dayN)][,"SPE_Flag"])
    u <- par("usr")
    if(length(which(pull.spe.t2.data[paste0(dayN)][,"SPE_Flag"] != 0)) > 0) text(0, 0.9,
                                                                             labels=format(index(pull.spe.t2.data[paste0(dayN)])[which(pull.spe.t2.data[paste0(dayN)][,"SPE_Flag"] != 0)[1]], "%H:%M"),
                                                                             col="red", pos = 4, cex=1.5)
    
    plotTimeseries(pull.spe.t2.data[paste0(dayN)][,"T2"])
    plotTimeseries(pull.spe.t2.data[paste0(dayN)][,"T2_Flag"])
    u <- par("usr")
    if(length(which(pull.spe.t2.data[paste0(dayN)][,"T2_Flag"] != 0)) > 0) text(0, 0.9,
                                                                                 labels=format(index(pull.spe.t2.data[paste0(dayN)])[which(pull.spe.t2.data[paste0(dayN)][,"T2_Flag"] != 0)[1]], "%H:%M"),
                                                                                 col="red", pos = 4, cex=1.5)


    par(mar=c(0,0,0,0))
    plot(0,0,type = "n",frame.plot = FALSE,axes = FALSE)
    u <- par("usr")
    text(0, u[4], labels = "Time (hours)",
         col = "black", pos = 1, cex=1.5)

    par(mar=c(0,0,0,0))
    plot(0,0,type = "n",frame.plot = FALSE,axes = FALSE)
    u <- par("usr")
    text(0, u[4], labels = "Time (hours)",
         col = "black", pos = 1, cex=1.5)
    
    par(mar=c(0,0,0,0))
    plot(0,0,type = "n",frame.plot = FALSE,axes = FALSE)
    u <- par("usr")
    text(0, u[4], labels = "Time (hours)",
         col = "black", pos = 1, cex=1.5)

  }
}
dev.off()
# 
# 
# 
# 
# 
# 
# ## PLOT RESULTS
# load(file="SS_foreach_results.RData")
# vars2plot <- c("BIO_BLOWER_1\\FLOW\\PROCESS_VALUE",
#                "MBR_1\\INF_FLOW\\PROCESS_VALUE",
#                "BIO_1\\LEVEL\\PROCESS_VALUE",
#                "BIO_1\\TSS\\PROCESS_VALUE")
# 
# alarm.results <- foreach(i=1:length(foreach.results),
#                          .combine = 'c') %dopar% { # For each day
#                            
#                            for(j in 1:length(foreach.results[[i]])) { # For each method
#                              foreach.results[[i]][[j]][[1]] <- cbind(foreach.results[[i]][[j]][[1]], rep(NA, nrow(foreach.results[[i]][[j]][[1]])))
#                              colnames(foreach.results[[i]][[j]][[1]])[ncol(foreach.results[[i]][[j]][[1]])] <-"Alarm"
#                              for(k in 1:nrow(foreach.results[[i]][[j]][[1]])) {
#                                if((foreach.results[[i]][[j]][[1]][k,2] == 0) && (foreach.results[[i]][[j]][[1]][k,4] == 0)) foreach.results[[i]][[j]][[1]][k,5] <- 0
#                                if((foreach.results[[i]][[j]][[1]][k,2] == 0) && (foreach.results[[i]][[j]][[1]][k,4] == 1)) foreach.results[[i]][[j]][[1]][k,5] <- 1
#                                if((foreach.results[[i]][[j]][[1]][k,2] == 1) && (foreach.results[[i]][[j]][[1]][k,4] == 0)) foreach.results[[i]][[j]][[1]][k,5] <- 2
#                                if((foreach.results[[i]][[j]][[1]][k,2] == 1) && (foreach.results[[i]][[j]][[1]][k,4] == 1)) foreach.results[[i]][[j]][[1]][k,5] <- 3
#                              }
#                              # results <- as.data.frame(alarm.rate.calc(FaultChecks = xts(foreach.results[[i]][[j]][[1]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]][[1]]))), real.alarms = real.faults))
#                              # colnames(results) <- paste(names(foreach.results[[i]])[j])
#                              results <- as.data.frame(foreach.results[[i]][[j]][[1]])
#                              
#                              if(j==1) results.all <- list(results)
#                              if(j > 1) results.all <- c(results.all, list(results))
#                              
#                              names(results.all)[length(names(results.all))] <- paste(names(foreach.results[[i]])[j])
#                            }
#                            results.list <- list(results.all)
#                            names(results.list) <- paste(names(foreach.results)[i])
#                            return(results.list)
#                          }
# 
# for(i in 1:length(foreach.results)) { # For each day
#   for(j in 1:length(foreach.results[[x]])) { # For each method
#     plot.index <- range(as.POSIXct(rownames(foreach.results[[i]][[j]][[1]])))
#     plot.data <- cbind(rawData[paste0(plot.index[1],"/",plot.index[2])], xts(foreach.results[[i]][[j]][[1]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]][[1]]))))
#     graph_alarmData(data=plot.data, keyword = paste0("plots//",names(foreach.results[[i]])[j]," ",names(foreach.results)[i]), fault.interval = real.faults, filetype = "png")
#     
#   }
# }
# 
# 
# ##### MULTISTATE #####
# train.and.test.data.2 <- rawData
# method.parameters <- list()
# method.parameters[[length(method.parameters)+1]] <- list(method="MS BR PCA", 
#                                                          dataCols=which(colnames(train.and.test.data.2) %in% varsBR),
#                                                          updateFreq = 1e10,
#                                                          Dynamic=FALSE)
# method.parameters[[length(method.parameters)+1]] <- list(method="MS MT PCA", 
#                                                          dataCols=which(colnames(train.and.test.data.2) %in% varsMT),
#                                                          updateFreq = 1e10,
#                                                          Dynamic=FALSE)
# 
# method.parameters[[length(method.parameters)+1]] <- list(method="MS BR D-PCA", 
#                                                          dataCols=which(colnames(train.and.test.data.2) %in% varsBR),
#                                                          updateFreq = 1e10,
#                                                          Dynamic=TRUE)
# method.parameters[[length(method.parameters)+1]] <- list(method="MS MT D-PCA", 
#                                                          dataCols=which(colnames(train.and.test.data.2) %in% varsMT),
#                                                          updateFreq = 1e10,
#                                                          Dynamic=TRUE)
# 
# method.parameters[[length(method.parameters)+1]] <- list(method="MS BR AD-PCA", 
#                                                          dataCols=which(colnames(train.and.test.data.2) %in% varsBR),
#                                                          updateFreq = 1440,
#                                                          Dynamic=TRUE)
# method.parameters[[length(method.parameters)+1]] <-
#   list(method="MS MT AD-PCA",
#        dataCols=which(colnames(train.and.test.data.2) %in% varsMT),
#        updateFreq = 1440,
#        Dynamic=TRUE)
# stateGenerator <- function(data,
#                            stateVars,
#                            vars,
#                            minObs = round((2*length(which(!(colnames(data) %in% stateVars))))^2/2),
#                            # testingDay = NULL,
#                            rollingWindowDays = 0) {
#   data <- data[,which(colnames(data) %in% c(stateVars,vars))]
#   # Determine the column numbers of the state variables
#   colNo <- which(colnames(data) %in% stateVars)
#   
#   # Returns character string of all state combinations
#   data <- cbind(data, as.numeric(do.call(paste,c(as.data.frame(data[,colNo]),sep=""))))
#   colnames(data)[length(colnames(data))] <- "labelCol"
#   
#   # Frequency distribution of each state?
#   if (rollingWindowDays == 0) {
#     freqDistribution <- as.data.frame(table(data$labelCol))
#   } else {
#     # freqDistribution <- as.data.frame(table(data$labelCol[paste((testingDay-rollingWindowDays),"/",(testingDay-1),sep="")]))
#     freqDistribution <- as.data.frame(table(data$labelCol[paste(as.Date(index(data)[1]),"/",(as.Date(index(data)[1]) + rollingWindowDays),sep="")]))
#     
#   }
#   
#   # Is the frequency of each state within the subset greater than the minimum during the training period?
#   statesToKeep <- as.numeric(as.character(freqDistribution[which(freqDistribution[,2] >= minObs),1]))
#   
#   # Test to ensure at least one state is included
#   if (identical(statesToKeep, numeric(0))) {
#     print("Insufficient datapoints to train. Increase rolling window size.")
#     # return(data)
#   } else {
#     # Only keep the states with sufficient datapoints
#     dataIndex <- which(data$labelCol %in% statesToKeep)
#     dataNew <- data[dataIndex,-colNo]
#     dataNew <- na.omit(dataNew)
#     dataNew <- dataNew[apply(dataNew, 1, function(x) all(is.finite(x))),]
#     data_ls <- list()
#     for (i in 1:length(statesToKeep)) {
#       dataHolder <- dataNew[which(dataNew$labelCol == statesToKeep[i]),]
#       if (rollingWindowDays == 0) {
#         data_ls <- c(data_ls, list(dataHolder))
#       } else {
#         data_ls <- c(data_ls, list(uniquenessCheck(dataHolder)))
#       }
#       
#     }
#     return(data_ls)
#   }
#   
#   
# }
# 
# 
# # # Train and test each rolling window
# # foreach.results <- list()
# # for(training.days in rollingWindowDays) {
# library(doParallel)
# library(foreach)
# numCores <- detectCores()
# registerDoParallel(numCores)
# foreach.results <- foreach(training.days=rollingWindowDays,
#                            .combine = 'c',
#                            # .multicombine=TRUE,
#                            # .init=list(list(), list()),
#                            .packages = c("mvMonitoringv2",
#                                          "ADPCA")) %dopar% {
#                                            
#                                            train.and.test.data.br <- stateGenerator(data = train.and.test.data.2,
#                                                                                     stateVars = stateVarsBR, 
#                                                                                     vars=varsBR,
#                                                                                     rollingWindowDays = training.days)
#                                            
#                                            train.and.test.data.mt <- stateGenerator(data = train.and.test.data.2,
#                                                                                     stateVars = stateVarsMT, 
#                                                                                     vars=varsMT,
#                                                                                     rollingWindowDays = training.days)
#                                            
#                                            for(r.method in 1:length(method.parameters)) {
#                                              
#                                              if(length(grep("BR", method.parameters[[r.method]][1]))==1) {
#                                                train.and.test.data.loop <- train.and.test.data.br
#                                              } else {
#                                                train.and.test.data.loop <- train.and.test.data.mt
#                                              }
#                                              if(is.character(train.and.test.data.loop)) break
#                                              
#                                              results.all <- vector()
#                                              
#                                              for(i in 1:length(train.and.test.data.loop)) { # For each state
#                                                
#                                                testing.stat <- "SPE"
#                                                data <- train.and.test.data.loop[[i]]
#                                                lets.try.something <- c(ncol(train.and.test.data.loop[[i]]))
#                                                error.index <- "EIGEN ERROR"
#                                                while(error.index == "EIGEN ERROR") {
#                                                  try(expr = {
#                                                    train1A_ls <- mspTrain(
#                                                      data = data[,-lets.try.something],
#                                                      trainObs = nrow(data[paste0("/",dayOne+training.days-1)]),
#                                                      labelVector = rep(1, nrow(data)), 
#                                                      updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
#                                                      Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
#                                                      faultsToTriggerAlarm = 3,
#                                                      statistic=testing.stat
#                                                    )
#                                                    error.index <- "FINISHED"
#                                                  }, silent=FALSE)
#                                                  if(error.index == "EIGEN ERROR") {
#                                                    remove.index <- min(apply(data[,-lets.try.something],2,function(x) length(unique(x))));
#                                                    lets.try.something <- c(lets.try.something, which(apply(data,2,function(x) length(unique(x))) <= remove.index))
#                                                  }
#                                                }
#                                                alarm.results <- as.data.frame(train1A_ls$FaultChecks[,grep(testing.stat, colnames(train1A_ls$FaultChecks))])
#                                                
#                                                testing.stat <- "T2"
#                                                lets.try.something <- c(ncol(train.and.test.data.loop[[i]]))
#                                                error.index <- "EIGEN ERROR"
#                                                while(error.index == "EIGEN ERROR") {
#                                                  try(expr = {
#                                                    train1A_ls <- mspTrain(
#                                                      data = data[,-lets.try.something],
#                                                      trainObs = nrow(data[paste0("/",dayOne+training.days-1)]),
#                                                      labelVector = rep(1, nrow(data)), 
#                                                      updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
#                                                      Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
#                                                      faultsToTriggerAlarm = 3,
#                                                      statistic=testing.stat
#                                                    )
#                                                    error.index <- "FINISHED"
#                                                  }, silent=FALSE)
#                                                  if(error.index == "EIGEN ERROR") {
#                                                    remove.index <- min(apply(data[,-lets.try.something],2,function(x) length(unique(x))));
#                                                    lets.try.something <- c(lets.try.something, which(apply(data,2,function(x) length(unique(x))) <= remove.index))
#                                                  }
#                                                }
#                                                alarm.results <- cbind(alarm.results, as.data.frame(train1A_ls$FaultChecks[,grep(testing.stat, colnames(train1A_ls$FaultChecks))]))
#                                                if(i == 1) alarm.results.state <- alarm.results
#                                                if(i > 1) alarm.results.state <- rbind(alarm.results.state, alarm.results)
#                                              }
#                                              
#                                              if(r.method==1) alarm.results.method <- list(alarm.results.state)
#                                              if(r.method>1) alarm.results.method <- c(alarm.results.method, list(alarm.results.state))
#                                              names(alarm.results.method)[[length(alarm.results.method)]] <- paste(as.character(unlist(method.parameters[[r.method]][1])))
#                                            }
#                                            results <- list(alarm.results.method)
#                                            names(results) <- paste(training.days,"days")
#                                            return(results)
#                                            # foreach.results <- c(results, results)
#                                          }
# save(foreach.results, file="MS_foreach_results")
# 
# alarm.results <- foreach(i=1:length(foreach.results),
#                          .combine = 'c') %dopar% { # For each day
#                            
#                            for(j in 1:length(foreach.results[[i]])) { # For each method
#                              foreach.results[[i]][[j]] <- cbind(foreach.results[[i]][[j]], rep(NA, nrow(foreach.results[[i]][[j]])))
#                              colnames(foreach.results[[i]][[j]])[ncol(foreach.results[[i]][[j]])] <-"Alarm"
#                              for(k in 1:nrow(foreach.results[[i]][[j]])) {
#                                if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 0
#                                if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 1
#                                if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 2
#                                if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 3
#                              }
#                              results <- as.data.frame(alarm.rate.calc(FaultChecks = xts(foreach.results[[i]][[j]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]]))), real.alarms = real.faults))
#                              colnames(results) <- paste(names(foreach.results[[i]])[j])
#                              
#                              if(j==1) results.all <- results
#                              if(j > 1) results.all <- cbind(results.all, results)
#                            }
#                            results.list <- list(results.all)
#                            names(results.list) <- paste(names(foreach.results)[i])
#                            return(results.list)
#                          }
# 
# 
# for(x in 1:length(alarm.results)) { # For each day
#   pull.data <- as.data.frame(alarm.results[[x]])
#   colnames(pull.data) <- paste(colnames(pull.data), names(alarm.results)[x])
#   if(x==1) results.cbind <- pull.data
#   if(x>1) results.cbind <- cbind(results.cbind, pull.data)
# }
# write.xlsx(results.cbind, file="MS_results_all.xlsx")
# 
# for(x in 1:length(foreach.results)) { # For each day
#   for(y in 1:length(foreach.results[[x]])) { # For each method
#     pull.data <- foreach.results[[x]][[y]]
#     # write.xlsx(pull.data, file="MS_results_all.xlsx", sheetName=paste(names(foreach.results[[x]])[y], names(foreach.results)[x]), append=TRUE, row.names=TRUE)
#     write.csv(pull.data, file=paste(names(foreach.results[[x]])[y], names(foreach.results)[x],".csv"))
#     # print(nrow(pull.data))
#   }
# }
# 
# 
# 
# 
# load(file="MS_foreach_results.RData")
# alarm.results <- foreach(i=1:length(foreach.results),
#                          .combine = 'c') %dopar% { # For each day
#                            
#                            for(j in 1:length(foreach.results[[i]])) { # For each method
#                              foreach.results[[i]][[j]] <- cbind(foreach.results[[i]][[j]], rep(NA, nrow(foreach.results[[i]][[j]])))
#                              colnames(foreach.results[[i]][[j]])[ncol(foreach.results[[i]][[j]])] <-"Alarm"
#                              for(k in 1:nrow(foreach.results[[i]][[j]])) {
#                                if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 0
#                                if((foreach.results[[i]][[j]][k,2] == 0) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 1
#                                if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 0)) foreach.results[[i]][[j]][k,5] <- 2
#                                if((foreach.results[[i]][[j]][k,2] == 1) && (foreach.results[[i]][[j]][k,4] == 1)) foreach.results[[i]][[j]][k,5] <- 3
#                              }
#                              
#                              results <- as.data.frame(foreach.results[[i]][[j]])
#                              
#                              if(j==1) results.all <- list(results)
#                              if(j > 1) results.all <- c(results.all, list(results))
#                              
#                              names(results.all)[length(names(results.all))] <- paste(names(foreach.results[[i]])[j])
#                            }
#                            results.list <- list(results.all)
#                            names(results.list) <- paste(names(foreach.results)[i])
#                            return(results.list)
#                          }
# stopImplicitCluster()
# for(i in 1:length(foreach.results)) { # For each day
#   for(j in 1:length(foreach.results[[x]])) { # For each method
#     plot.index <- range(as.POSIXct(rownames(foreach.results[[i]][[j]])))
#     plot.data <- cbind(rawData[paste0(plot.index[1],"/",plot.index[2])], xts(foreach.results[[i]][[j]], order.by = as.POSIXct(rownames(foreach.results[[i]][[j]]))))
#     graph_alarmData(data=plot.data, keyword = paste0("plots//",names(foreach.results[[i]])[j]," ",names(foreach.results)[i]), fault.interval = real.faults, filetype = "png")
#   }
# }
# 

