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


### Train ADPCA model
### Run multistate function for BR and MT, return xts with test data
trainingDataBR <- multistate_train(rawData = rawData,
                                   vars = varsBR,
                                   stateVars = stateVarsBR,
                                   testingDay = testingDay,
                                   rollingWindowDays = rollingWindowDays,
                                   alphaN = alphaN,
                                   faultsToTriggerAlarm = faultsToTriggerAlarm)

trainingDataMT <- multistate_train(rawData = rawData,
                                   vars = varsMT,
                                   stateVars = stateVarsMT,
                                   testingDay = testingDay,
                                   rollingWindowDays = rollingWindowDays,
                                   alphaN = alphaN,
                                   faultsToTriggerAlarm = faultsToTriggerAlarm)

trainingDataSS <- createTrainingSpecs(data = uniqueData,
                                      testingDay = testingDay,
                                      rollingWindowDays = rollingWindowDays,
                                      alpha = alphaN,
                                      faultsToTriggerAlarm = faultsToTriggerAlarm)

# Create states
dataBR_ls <- stateGenerator(data = dataBR, stateVars = stateVarsBR, testingDay = testingDay, minObs = 1)

dataMT_ls <- stateGenerator(data = dataMT, stateVars = stateVarsMT, testingDay = testingDay, minObs = 1)

states2keepBR <- as.vector(sapply(trainingDataBR[[1]][[1]], function(x) x[1,ncol(x)]))
states2keepMT <- as.vector(sapply(trainingDataMT[[1]][[1]], function(x) x[1,ncol(x)]))

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

filtered.dataMT_ls <- list()
for (j in 1:length(states2keepMT)) {
  for (i in 1:length(dataMT_ls)) {
    n <- dataMT_ls[[i]]$labelCol[1]
    if (n == states2keepMT[j]) {
      filtered.dataMT_ls <- c(filtered.dataMT_ls, list(dataMT_ls[[i]]))
    } else {}
  }
}

### Test ADPCA model
# Test SS
alarmDataSS <- testNewObs(data = rawData,
                          trainingSpecs = trainingDataSS,
                          testingDay = testingDay,
                          faultsToTriggerAlarm = faultsToTriggerAlarm)

# Test multistate
alarmDataBR <- multistate_test(data = filtered.dataBR_ls,
                               trainingSpec_ls = trainingDataBR[[2]][[1]],
                               testingDay = trainingDataBR[[3]],
                               faultsToTriggerAlarm = trainingDataBR[[4]])

# Insufficient observations for this case study to do use data for MT
alarmDataMT <- multistate_test(data = filtered.dataMT_ls,
                               trainingSpec_ls = trainingDataMT[[2]][[1]],
                               testingDay = trainingDataMT[[3]],
                               faultsToTriggerAlarm = trainingDataMT[[4]])