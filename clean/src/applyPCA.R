applyPCA <- function(train.data, test.data, per.var, metric, alpha) {
  if(length(test.data)==0) return(NA)
  
  require(BMS)
  
  # Scale data
  sigmaTrain <- cov(train.data)
  
  # If the SD = 0, var should be removed (can't calculate eigenvector)
  if(length(which(diag(sigmaTrain)==0))>0) { 
    n <- which(diag(sigmaTrain)==0)
    train.data <- train.data[,-n]
    test.data <- test.data[,-n]
    sigmaTrain <- cov(train.data)
  }
  muTrain <- colMeans(train.data)
  scaledtrain.data <- scale(train.data, center=muTrain, scale=diag(sigmaTrain))
  
  # Calculate eigenvectors
  stdDevs <- sqrt(diag(sigmaTrain))
  precisRootMat <- diag(1 / stdDevs, ncol = ncol(sigmaTrain))
  
  muTrain_mat <- rep(1, nrow(test.data)) %*% t(muTrain)
  scaledTest <- as.matrix(test.data - muTrain_mat) %*% precisRootMat
  scaledTest <- xts(scaledTest, order.by = index(test.data))
  colnames(scaledTest) <- colnames(test.data)
  
  ### Calculate spe/t2 thresholds
  R <- cor(scaledtrain.data, use = "pairwise.complete.obs")
  eigenR <- eigen(R)
  evalR <- eigenR$values
  evecR <- eigenR$vectors
  
  prop.var <- as.matrix(cumsum(evalR) / sum(evalR) * 100)
  comps <- last(which(prop.var - (per.var * 100) < 0))
  P <- as.matrix(evecR[, 1:comps]) # Transformation matrix
  Lambda <- diag(evalR[1:comps], ncol = length(1:comps))
  
  # Rotated Matrix
  PCs <- scaledtrain.data %*% P
  # Reduced Matrix in Original Space
  X.hat <- PCs %*% t(P)
  # Residual Matrix
  E <- scaledtrain.data - X.hat
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
  scaledTest.results <- matrix("IC", nrow=nrow(scaledTest), ncol = 3)
  scaledTest.metrics <- matrix(NA, nrow=nrow(scaledTest), ncol = 4)
  colnames(scaledTest.results) <- c("SPE", "T2", "SPE-T2")
  for(row in 1:nrow(scaledTest)) {
    observation <- scaledTest[row,]
    proj_observation <- observation %*% P
    # Reduced Observation in Original Space
    obs.hat <- proj_observation %*% t(P)
    # Residual Vector
    E <- observation - obs.hat
    # Squared prediction error monitoring statistic
    SPE <- diag(E %*% t(E))
    if(SPE>SPE.np.lim) scaledTest.results[row, 1] <- "OC"
    # Hotelling's T^2 monitoring statistic
    T2 <- diag(proj_observation %*% LambdaInv %*% t(proj_observation))
    if(T2>T2.np.lim) scaledTest.results[row, 2] <- "OC"
    if((T2>T2.np.lim) && (SPE>SPE.np.lim)) scaledTest.results[row, 3] <- "OC"
    scaledTest.metrics[row,1] <- SPE
    scaledTest.metrics[row,2] <- SPE.np.lim
    scaledTest.metrics[row,3] <- T2
    scaledTest.metrics[row,4] <- T2.np.lim
  }
  
  if(metric=="SPE") return.col <- 1
  if(metric=="T2") return.col <- 2
  if(metric=="SPE-T2") return.col <- 3
  
  return.all <- list(xts(scaledTest.results[,return.col], order.by = index(scaledTest)),
                     xts(scaledTest.metrics, order.by=index(scaledTest)))
  
  return(return.all) # Changed this to return a list with metric and threshold
}