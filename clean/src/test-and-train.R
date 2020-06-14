train.and.test <- function(rawData, 
                           trainObs, 
                           # dynamic = TRUE,
                           var.amnt = 0.9, 
                           alpha = 0.01) {
  require(BMS)
  
  # Split training and testing data
  # if(dynamic) rawData <- lag(x=rawData, k=c(0,1))[2:nrow(rawData)] # lag for dynamic adaptation
  trainData <- rawData[1:trainObs,] # set training data
  testData <- rawData[(trainObs+1):nrow(rawData)]
  
  # Scale data
  sigmaTrain <- cov(trainData)
  
  # If the SD = 0, var should be removed (can't calculate eigenvector)
  if(length(which(diag(sigmaTrain)==0))>0) { 
    n <- which(diag(sigmaTrain)==0)
    trainData <- trainData[,-n]
    testData <- testData[,-n]
    sigmaTrain <- cov(trainData)
  }
  muTrain <- colMeans(trainData)
  scaledTrainData <- scale(trainData, center=muTrain, scale=diag(sigmaTrain))
  
  # Calculate eigenvectors
  stdDevs <- sqrt(diag(sigmaTrain))
  precisRootMat <- diag(1 / stdDevs, ncol = ncol(sigmaTrain))
  
  muTrain_mat <- rep(1, nrow(testData)) %*% t(muTrain)
  scaledTest <- as.matrix(testData - muTrain_mat) %*% precisRootMat
  scaledTest <- xts(scaledTest, order.by = index(testData))
  colnames(scaledTest) <- colnames(testData)
  
  ### Calculate spe/t2 thresholds
  R <- cor(scaledTrainData, use = "pairwise.complete.obs")
  eigenR <- eigen(R)
  evalR <- eigenR$values
  evecR <- eigenR$vectors
  
  prop.var <- as.matrix(cumsum(evalR) / sum(evalR) * 100)
  comps <- last(which(prop.var - (var.amnt * 100) < 0))
  P <- as.matrix(evecR[, 1:comps]) # Transformation matrix
  Lambda <- diag(evalR[1:comps], ncol = length(1:comps))
  
  # Rotated Matrix
  PCs <- scaledTrainData %*% P
  # Reduced Matrix in Original Space
  X.hat <- PCs %*% t(P)
  # Residual Matrix
  E <- scaledTrainData - X.hat
  # Squared prediction error monitoring statistic
  SPEs <- diag(E %*% t(E))
  # Hotelling's T^2 monitoring statistic
  LambdaInv <- solve(Lambda)
  T2s <- diag(PCs %*% LambdaInv %*% t(PCs))
  # Calculate thresholds
  SPE.np.d <- density(SPEs, bw = "SJ", kernel = "gaussian", from = 0)
  SPE.np.lim <- BMS::quantile.density(SPE.np.d, 1 - alpha)
  T2.np.d <- density(T2s, bw = "SJ", kernel = "gaussian", from = 0)
  T2.np.lim <- BMS::quantile.density(T2.np.d, 1 - alpha)
  
  ### Calculate pca/spe/t2 on testing
  scaledTest.results <- matrix(NA, nrow=nrow(scaledTest), ncol = 6)
  colnames(scaledTest.results) <- c("SPE", "SPE_Limit", "SPE_Flag", "T2", "T2_Limit", "T2_Flag")
  for(row in 1:nrow(scaledTest)) {
    observation <- scaledTest[row,]
    proj_observation <- observation %*% P
    # Reduced Observation in Original Space
    obs.hat <- proj_observation %*% t(P)
    # Residual Vector
    E <- observation - obs.hat
    # Squared prediction error monitoring statistic
    SPE <- diag(E %*% t(E))
    SPE_flag <- as.numeric(SPE > SPE.np.lim)
    # Hotelling's T^2 monitoring statistic
    T2 <- diag(proj_observation %*% LambdaInv %*% t(proj_observation))
    T2_flag <- as.numeric(T2 > T2.np.lim)
    object <- matrix(c(SPE, SPE.np.lim, SPE_flag, T2, T2.np.lim, T2_flag), nrow = 1)
    scaledTest.results[row,] <- object
  }
  
  # # Find rows to keep
  # alarm.spe <- vector()
  # alarm.t2 <- vector()
  # for(j in faultToTriggerAlarm:nrow(scaledTest.results)) {
  #   sum.spe <- sum(scaledTest.results[(j-faultToTriggerAlarm+1):j,3])
  #   sum.t2 <- sum(scaledTest.results[(j-faultToTriggerAlarm+1):j,6])
  #   
  #   if(sum.spe == faultToTriggerAlarm) alarm.spe <- c(alarm.spe, j)
  #   if(sum.t2 == faultToTriggerAlarm) alarm.t2 <- c(alarm.t2, j)
  # }
  # 
  # Alarms <- rep(0,nrow(scaledTest.results))
  # Alarms[1:(faultToTriggerAlarm-1)] <- NA
  # if(length(alarm.spe) > 0) Alarms[alarm.spe] <- 1
  # if(length(alarm.t2) > 0) Alarms[alarm.t2] <- 2
  # if((length(alarm.spe) > 0) && (length(alarm.t2) > 0)) {
  #   if(any(alarm.t2 %in% alarm.spe)) Alarms[alarm.t2[which(alarm.t2 %in% alarm.spe)]] <- 3
  # }
  # 
  # return(xts(cbind(scaledTest.results, Alarms), order.by = index(scaledTest)))
  return(xts(scaledTest.results, order.by = index(scaledTest)))
}
