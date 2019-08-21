library(mvMonitoring)
library(MSQC) ## for MEWMA and MCUSUM
library(qcc) ## for univariate monitoring

## data set from mvMonitoring package
head(fault1A_xts)

###################################
## Option 1: PCA
## Single state, non-adaptive,
## not dynamic
###################################
train1A_xts <- fault1A_xts[1:8460, -1]
train1A_ls <- mspTrain(
  data = train1A_xts,
  trainObs = 3 * 24 * 60,
  labelVector = rep(1, nrow(train1A_xts)), ## single state
  updateFreq = 1e10, ## never updates
  Dynamic = FALSE, ## not dynamic
  lagsIncluded = 0, ## don't include lagged variables
  faultsToTriggerAlarm = 5
)
test1A_xts <- fault1A_xts[8461:8520, -1]
monitor1A_xts <- mspMonitor(
  observations = test1A_xts,
  labelVector = rep(1, nrow(test1A_xts)),
  trainingSummary = train1A_ls$TrainingSpecs
)

head(monitor1A_xts)


###################################
## Option 2: MCUSUM/MEWMA
###################################

## data must be a matrix or data frame
fault1A <- as.data.frame(fault1A_xts)
str(fault1A)

## training/test data
train1A <- fault1A[1:8460, -1]
test1A <- fault1A[8461:8520, -1]

## estimate mean and covariance using training data
Xmv <- mult.chart(train1A, type = "t2")$Xmv
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