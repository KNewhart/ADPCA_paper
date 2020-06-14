control.chart <- function(train.data, test.data, limits) {
  # Do not include observations when the system is offline
  train.data <- train.data[which(train.data[,"MBR_1\\CURRENT_STATE"]==1),]
  train.data <- train.data[which(train.data[,"MBR_1\\CURRENT_MODE"]==1),]
  train.data <- train.data[which(train.data[,"MBR_2\\CURRENT_STATE"]==1),]
  train.data <- train.data[which(train.data[,"MBR_2\\CURRENT_MODE"]==1),]
  train.data <- train.data[which(train.data[,"MBR\\CURRENT_FLUX_MODE"]==0)]
  train.data <- train.data[which(train.data[,"BIO_1\\CURRENT_PHASE"]>0),]
  train.data <- train.data[which(train.data[,"BIO_2\\CURRENT_PHASE"]>0),]
  
  test.data <- test.data[which(test.data[,"MBR_1\\CURRENT_STATE"]==1),]
  test.data <- test.data[which(test.data[,"MBR_1\\CURRENT_MODE"]==1),]
  test.data <- test.data[which(test.data[,"MBR_2\\CURRENT_STATE"]==1),]
  test.data <- test.data[which(test.data[,"MBR_2\\CURRENT_MODE"]==1),]
  test.data <- test.data[which(test.data[,"MBR\\CURRENT_FLUX_MODE"]==0)]
  test.data <- test.data[which(test.data[,"BIO_1\\CURRENT_PHASE"]>0),]
  test.data <- test.data[which(test.data[,"BIO_2\\CURRENT_PHASE"]>0),]
  
  
  
  # Only monitor process variables
  train.data <- train.data[,which(colnames(train.data) %in% c(varsBR, varsMT))]
  train.data <- train.data[,-(which(colnames(train.data) %in% c(stateVarsBR, stateVarsMT)))]
  test.data <- test.data[,which(colnames(test.data) %in% c(varsBR, varsMT))]
  test.data <- test.data[,-(which(colnames(test.data) %in% c(stateVarsBR, stateVarsMT)))]

  
  # Calculate upper and lower control limits
  avg.ls <- apply(train.data,2,mean)
  sd.ls <- apply(train.data,2,sd)
  ucl.ls <- avg.ls+limits*sd.ls
  lcl.ls <- avg.ls-limits*sd.ls
  
  results.mat <- matrix(data="IC", nrow=nrow(test.data), ncol=1)
  for(col in 1:ncol(test.data)) {
    if(length(which(test.data[,col] < lcl.ls[col]))>0) {
      results.mat[which(test.data[,col] < lcl.ls[col]),1] <- "OC"
    }

    if(length(which(test.data[,col] > ucl.ls[col]))>0) {
      results.mat[which(test.data[,col] > ucl.ls[col]),1] <- "OC"
    }
  }
  
  return(xts(results.mat, order.by=index(test.data)))
}