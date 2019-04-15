just.alarms <- list()
# days <- 25
i = 1
testingDay.org <- testingDay

for (i in 1:days) {
  # Move testing day forward one day in loop
  testingDay <- testingDay.org + i - 1
  testedData.list <- list()
  if (i != 1) {
    load(file = "trainingDataBR.R")
    # load(file = "trainingDataMT.R")
    load(file = "trainingDataSS.R")
    
    # trainingData.list <- list(trainingDataSS, trainingDataBR, trainingDataMT)
    # alarmData.list <- list(alarmDataSS, alarmDataBR, alarmDataMT)
    trainingData.list <- list(trainingDataSS, trainingDataBR)
    alarmData.list <- list(alarmDataSS, alarmDataBR)
    index.list <- list()
    
    for (k in 1:length(trainingData.list)) {
      # Merge different states
      if (is.list(trainingData.list[[k]][[1]][[1]])) {
        blah <- do.call("merge",trainingData.list[[k]][[1]][[1]])
      } else {
        blah <- trainingData.list[[k]][[1]]
      }
      index.blah <- index(blah)
      # for (j in grep("PROCESS_VALUE.1", colnames(blah))) {
      #   rows2transfer <- which(!is.na(blah[,j]))
      #   col2transfer2 <- grep(strsplit(colnames(blah)[j], "PROCESS_VALUE.1")[[1]], colnames(blah))[1]
      #   blah[rows2transfer, col2transfer2] <- blah[rows2transfer, j]
      # }
      # blah <- blah[,c(-grep("PROCESS_VALUE.1", colnames(blah)))]
      # 
      # # Merge labelCol
      # rows2transfer <- which(!is.na(blah$labelCol.1))
      # col2transferfrom <- which(colnames(blah) == "labelCol.1")
      # col2transfer2 <- which(colnames(blah) == "labelCol")
      # blah[rows2transfer, col2transfer2] <- blah[rows2transfer, col2transferfrom]
      # blah <- blah[,-col2transferfrom]
      # 
      # Import clean data from previous day
      alarm.col <- which(colnames(alarmData.list[[k]]) == "Alarm")
      index.blah <- c(index.blah, index(alarmData.list[[k]][which(alarmData.list[[k]][,alarm.col] == 0),]))
      # blah2 <- alarmData.list[[k]][which(alarmData.list[[k]][,alarm.col] == 0),c(-grep("PROCESS_VALUE.1", colnames(alarmData.list[[k]])))]
      # colnames2remove <- c("SPE","SPE_Flag","T2","T2_Flag","Alarm","SPE_threshold","T2_threshold")
      # blah2 <- blah2[,-c(which(colnames(blah2) %in% colnames2remove))]
      # 
      # # Merge different states
      # blah <- merge(blah, blah2)
      # for (j in grep("PROCESS_VALUE.1", colnames(blah))) {
      #   rows2transfer <- which(!is.na(blah[,j]))
      #   col2transfer2 <- grep(strsplit(colnames(blah)[j], "PROCESS_VALUE.1")[[1]], colnames(blah))[1]
      #   blah[rows2transfer, col2transfer2] <- blah[rows2transfer, j]
      # }
      # blah <- blah[,c(-grep("PROCESS_VALUE.1", colnames(blah)))]
      # 
      # # Merge labelCol
      # rows2transfer <- which(!is.na(blah$labelCol.1))
      # col2transferfrom <- which(colnames(blah) == "labelCol.1")
      # col2transfer2 <- which(colnames(blah) == "labelCol")
      # blah[rows2transfer, col2transfer2] <- blah[rows2transfer, col2transferfrom]
      # blah <- blah[,-col2transferfrom]
      # 
      # testedData.list[[k]] <- blah
      index.list[[k]] <- index.blah
    }
  }
  
  
  ### Compile & clean raw data
  # loadandcleanDBF returns a dataframe with all days including column names
  rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)
  # convert to xts
  rawData <- xts(rawData[,-1], order.by = rawData[,1])
  rawData <- rawData[paste("/",testingDay,sep="")]
  # Subset data into BR and and MT
  dataBR <- rawData[,varsBR]
  dataMT <- rawData[,varsMT]
  # Clean single state data
  uniqueData <- uniquenessCheck(rawData)
  # Generate 'labelCol'
  uniqueData <- cbind(uniqueData, rep(1,nrow(uniqueData)))
  colnames(uniqueData)[ncol(uniqueData)] <- "labelCol"
  
  if (length(testedData.list) != 0) {
    index.ss <- index.list[[1]][which(index.list[[1]] > paste(testingDay - nDays))]
    if (range(index.ss)[1] > (paste(testingDay - nDays))) {
      index.unique <- index(uniqueData)
      index.unique.before <- index.unique[which(index.unique < range(index.ss)[1])]
      index.ss <- c(index.unique.before, index.ss)
    }
    index.br <- index.list[[2]][which(index.list[[2]] > paste(testingDay - nDays))]
    # index.mt <- index.list[[3]][which(index.list[[3]] > paste(testingDay - nDays))]
    
  } else {
    index.ss <- index(uniqueData)
    index.br <- index(rawData)
    # index.mt <- index(rawData)
  }

  ### Train ADPCA model
  ### Run multistate function for BR and MT, return xts with test data
  trainingDataBR <- multistate_train(rawData = rawData[index.br],
                                     vars = varsBR,
                                     stateVars = stateVarsBR,
                                     testingDay = testingDay,
                                     rollingWindowDays = rollingWindowDays,
                                     alphaN = alphaN,
                                     faultsToTriggerAlarm = faultsToTriggerAlarm)
  
  # trainingDataMT <- multistate_train(rawData = rawData[index.mt],
  #                                    vars = varsMT,
  #                                    stateVars = stateVarsMT,
  #                                    testingDay = testingDay,
  #                                    rollingWindowDays = rollingWindowDays,
  #                                    alphaN = alphaN,
  #                                    faultsToTriggerAlarm = faultsToTriggerAlarm)
  
  trainingDataSS <- createTrainingSpecs(data = uniqueData[index.ss],
                                        testingDay = testingDay,
                                        rollingWindowDays = rollingWindowDays,
                                        alpha = alphaN,
                                        faultsToTriggerAlarm = faultsToTriggerAlarm)
  
  save(trainingDataBR, file = "trainingDataBR.R")
  # save(trainingDataMT, file = "trainingDataMT.R")
  save(trainingDataSS, file = "trainingDataSS.R")
  
  # Create states
  dataBR_ls <- stateGenerator(data = dataBR, stateVars = stateVarsBR, testingDay = testingDay, minObs = 1)
  
  # dataMT_ls <- stateGenerator(data = dataMT, stateVars = stateVarsMT, testingDay = testingDay, minObs = 1)
  
  states2keepBR <- as.vector(sapply(trainingDataBR[[1]][[1]], function(x) x[1,ncol(x)]))
  # states2keepMT <- as.vector(sapply(trainingDataMT[[1]][[1]], function(x) x[1,ncol(x)]))
  
  # Only include states with training data
  filtered.dataBR_ls <- list()
  for (j in 1:length(states2keepBR)) {
    for (i in 1:length(dataBR_ls)) {
      n <- dataBR_ls[[i]]$labelCol[1]
      if (n == states2keepBR[j]) {
        filtered.dataBR_ls <- c(filtered.dataBR_ls, list(dataBR_ls[[i]]))
      } else {}
    }
  }
  
  # filtered.dataMT_ls <- list()
  # for (j in 1:length(states2keepMT)) {
  #   for (i in 1:length(dataMT_ls)) {
  #     n <- dataMT_ls[[i]]$labelCol[1]
  #     if (n == states2keepMT[j]) {
  #       filtered.dataMT_ls <- c(filtered.dataMT_ls, list(dataMT_ls[[i]]))
  #     } else {}
  #   }
  # }
  
  ### Test ADPCA model
  # Test SS
  alarmDataSS <- testNewObs(data = rawData,
                            trainingSpecs = trainingDataSS,
                            testingDay = testingDay,
                            faultsToTriggerAlarm = faultsToTriggerAlarm)
  
  # Test multistate
  tryCatch(alarmDataBR <- multistate_test(data = filtered.dataBR_ls,
                                          trainingSpec_ls = trainingDataBR[[2]][[1]],
                                          testingDay = trainingDataBR[[3]],
                                          faultsToTriggerAlarm = trainingDataBR[[4]]),
           error = function(e) {
             alarmDataBR <- alarmDataSS$Alarm
             alarmDataBR$Alarm <- 0
           })
  
  # Insufficient observations for this case study to do use data for MT, skip if error
  
  # tryCatch(alarmDataMT <- multistate_test(data = filtered.dataMT_ls,
  #                                         trainingSpec_ls = trainingDataMT[[2]][[1]],
  #                                         testingDay = trainingDataMT[[3]],
  #                                         faultsToTriggerAlarm = trainingDataMT[[4]]),
  #          error = function(e) {
  #            alarmDataMT <- alarmDataBR$Alarm
  #            alarmDataMT$Alarm <- 0
  #          })
  
  
  save(alarmDataBR, file = "alarmDataBR.R")
  # tryCatch(save(alarmDataMT, file = "alarmDataMT.R"),
  #          error = function(e) {})
  save(alarmDataSS, file = "alarmDataSS.R")
  
  # # Plot of testing data
  # graph_alarmData(data = alarmDataSS[,c(-grep("PROCESS_VALUE.1", colnames(alarmDataSS)))],
  #                 keyword = paste0("results/",keyword,"_alarmDataSS"))
  # graph_alarmData(data = alarmDataBR[,c(-grep("PROCESS_VALUE.1", colnames(alarmDataBR)))],
  #                 keyword = paste0("results/",keyword,"_alarmDataBR"))
  # graph_alarmData(data = alarmDataMT[,c(-grep("PROCESS_VALUE.1", colnames(alarmDataMT)))],
  #                 keyword = paste0("results/",keyword,"_alarmDataMT"))
  
  # # Plot of alarm data
  {
    # daterange <- c(as.POSIXct(paste(testingDay,"00:00:00 UTC")), as.POSIXct(paste(testingDay+1,"00:00:00 UTC")))
    # objectlist <- list(alarmDataSS, alarmDataBR, alarmDataMT)
    # titlelist <- c("SS Alarms", "BR Alarms", "MT Alarms")
    # filename <- paste0("results/",keyword, " alarmPlots ",testingDay)
    # plot.alarm.data <- function(){
    #   par(mfrow=c(3,1), mar = c(2.1, 3.1, 2.1, 2.1))
    #   for(i in 1:3) {
    #     a <- which(index(objectlist[[i]]) > daterange[1])
    #     a <- a[which(index(objectlist[[i]])[a] < daterange[2])]
    #     alarmCol <- which(colnames(objectlist[[i]]) == "Alarm")
    #     # Using zoo object because problem in plot.xts
    #     plot(as.zoo(objectlist[[i]][a,alarmCol][!is.na(objectlist[[i]][a,alarmCol])]),
    #          yaxt="n",
    #          ylab="",
    #          ylim=c(0,3),
    #          pch=19)
    #     #abline(v=trueFaultTime, col="blue")
    #     title(titlelist[i])
    #     axis(2, at=c(0,1,2,3), labels = c("No","SPE","T2","Both"))
    #   }
    # }
    # svg(filename = paste(filename, ".svg", sep=""), width = 8, height = 6, family = "serif")
    # plot.alarm.data()
    # dev.off()
  }
  
  # obj.list <- c(alarmDataSS, alarmDataBR, alarmDataMT)
  
  if (length(just.alarms) == 0) {
    # Plot to check for normal operating conditions (i.e., good training data)
    graph_rawData(rawData, paste0("results/",keyword))
    
    alarm.col <- which(colnames(alarmDataSS) == "Alarm")
    alarm.row <- which(alarmDataSS[,alarm.col] != 0)
    just.alarms[[1]] <- alarmDataSS[alarm.row, alarm.col]
    
    alarm.col <- which(colnames(alarmDataBR) == "Alarm")
    alarm.row <- which(alarmDataBR[,alarm.col] != 0)
    just.alarms[[2]] <- alarmDataBR[alarm.row, alarm.col]
    
    # tryCatch( {
    # alarm.col <- which(colnames(alarmDataMT) == "Alarm")
    # alarm.row <- which(alarmDataMT[,alarm.col] != 0)
    # just.alarms[[3]] <- alarmDataMT[alarm.row, alarm.col]},
    # error = function(e) {})
    
    
  } else {
    
    alarm.col <- which(colnames(alarmDataSS) == "Alarm")
    alarm.row <- which(alarmDataSS[,alarm.col] != 0)
    just.alarms[[1]] <- c(just.alarms[[1]], alarmDataSS[alarm.row, alarm.col])
    
    alarm.col <- which(colnames(alarmDataBR) == "Alarm")
    alarm.row <- which(alarmDataBR[,alarm.col] != 0)
    just.alarms[[2]] <- c(just.alarms[[2]], alarmDataBR[alarm.row, alarm.col])
    
    # tryCatch( {
    # alarm.col <- which(colnames(alarmDataMT) == "Alarm")
    # alarm.row <- which(alarmDataMT[,alarm.col] != 0)
    # just.alarms[[3]] <- c(just.alarms[[3]], alarmDataMT[alarm.row, alarm.col])
    # }, error = function(e) {})
    
  }
  
}

### Compile & clean raw data
# loadandcleanDBF returns a dataframe with all days including column names
rawData <- loadandcleanDBF(dataLocation, testingDay, days)
# rawData <- loadandcleanDBF(dataLocation, as.Date("2018-01-25"), 1)
# convert to xts
rawData <- xts(rawData[,-1], order.by = rawData[,1])
rawData <- rawData[paste("/",testingDay,sep="")]
# Subset data into BR and and MT
dataBR <- rawData[,varsBR]
# dataMT <- rawData[,varsMT]
# Clean single state data
uniqueData <- uniquenessCheck(rawData)
# Generate 'labelCol'
uniqueData <- cbind(uniqueData, rep(1,nrow(uniqueData)))
colnames(uniqueData)[ncol(uniqueData)] <- "labelCol"

ss <- merge(uniqueData, just.alarms[[1]])
ss$Alarm[is.na(ss$Alarm)] <- 0
# ss <- ss["2018-01-25/2018-01-26 00:00"]
br <- merge(dataBR, just.alarms[[2]])
br$Alarm[is.na(br$Alarm)] <- 0
# br <- br["2018-01-25/2018-01-26 00:00"]
# mt <- merge(dataMT, just.alarms[[3]])
# mt$Alarm[is.na(mt$Alarm)] <- 0
# mt <- mt["2018-01-25/2018-01-26 00:00"]
