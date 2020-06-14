adpca <- function(train.data, 
                  test.data, 
                  dynamic=TRUE,
                  multistate=TRUE,
                  alpha=0.01, 
                  per.var=0.99,
                  metric="SPE-T2") { # Run PCA on BR and MT
  
  processVarsBR <- varsBR[-which(varsBR %in% stateVarsBR)]
  processVarsMT <- varsMT[-which(varsMT %in% stateVarsMT)]
  
  if(multistate) {
    # State information
    state.train <- train.data[,which(colnames(train.data) %in% c(stateVarsBR, stateVarsMT))]
    state.train <- cbind("BR.State"=apply(state.train[,stateVarsBR],1,paste,collapse=""),
                         "MT.State"=apply(state.train[,stateVarsMT],1,paste,collapse=""))
    state.train <- xts(state.train, order.by=index(train.data))
    
    state.test <- test.data[,which(colnames(test.data) %in% c(stateVarsBR, stateVarsMT))]
    state.test <- cbind("BR.State"=apply(state.test[,stateVarsBR],1,paste,collapse=""),
                        "MT.State"=apply(state.test[,stateVarsMT],1,paste,collapse=""))
    state.test <- xts(state.test, order.by=index(test.data))
    
    if(dynamic) {
      n <- which(unlist(lapply(unique(state.train$BR.State), function(state) any(state.test$BR.State==state))))
      train.br.ls <- lapply(unique(state.train$BR.State)[n], 
                            function(state) lag(train.data[which(state.train$BR.State==state),which(colnames(train.data) %in% processVarsBR)], c(0,1))
      )
      test.br.ls <- lapply(unique(state.train$BR.State)[n], 
                           function(state) lag(test.data[which(state.test$BR.State==state),which(colnames(test.data) %in% processVarsBR)], c(0,1))
      )
      n <- which(unlist(lapply(unique(state.train$MT.State), function(state) any(state.test$MT.State==state))))
      train.mt.ls <- lapply(unique(state.train$MT.State)[n], 
                            function(state) lag(train.data[which(state.train$MT.State==state),which(colnames(train.data) %in% processVarsMT)], c(0,1))
      )
      test.mt.ls <- lapply(unique(state.train$MT.State)[n], 
                           function(state) lag(test.data[which(state.test$MT.State==state),which(colnames(test.data) %in% processVarsMT)], c(0,1))
      )
    } else {
      n <- which(unlist(lapply(unique(state.train$BR.State), function(state) any(state.test$BR.State==state))))
      train.br.ls <- lapply(unique(state.train$BR.State)[n], 
                            function(state) train.data[which(state.train$BR.State==state),which(colnames(train.data) %in% processVarsBR)]
      )
      test.br.ls <- lapply(unique(state.train$BR.State)[n], 
                           function(state) test.data[which(state.test$BR.State==state),which(colnames(test.data) %in% processVarsBR)]
      )
      n <- which(unlist(lapply(unique(state.train$MT.State), function(state) any(state.test$MT.State==state))))
      train.mt.ls <- lapply(unique(state.train$MT.State)[n], 
                            function(state) train.data[which(state.train$MT.State==state),which(colnames(train.data) %in% processVarsMT)]
      )
      test.mt.ls <- lapply(unique(state.train$MT.State)[n], 
                           function(state) test.data[which(state.test$MT.State==state),which(colnames(test.data) %in% processVarsMT)]
      )
    }
    
  } else {
    if(dynamic) {
      train.br.ls <- list(lag(train.data[,processVarsBR],  c(0,1)))
      test.br.ls <- list(lag(test.data[,processVarsBR], c(0,1)))
      train.mt.ls <- list(lag(train.data[,processVarsMT], c(0,1)))
      test.mt.ls <- list(lag(test.data[,processVarsMT],  c(0,1)))
    } else {
      train.br.ls <- list(train.data[,processVarsBR])
      test.br.ls <- list(test.data[,processVarsBR])
      train.mt.ls <-list(train.data[,processVarsMT])
      test.mt.ls <- list(test.data[,processVarsMT])
    }
  }
  
  # Apply PCA
  library(doParallel)
  cores <- detectCores(all.tests = FALSE, logical = TRUE)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # For length of BR list
  results.br.ls <- foreach(i=1:length(train.br.ls), .combine='c', 
                           .packages = c("xts"), 
                           .export = c("applyPCA")) %dopar% {
    if(nrow(na.omit(train.br.ls[[i]]))<ceiling(nrow(train.data)*0.01)) {
      return(NA)
    } else {
      return(list(applyPCA(train.data=na.omit(train.br.ls[[i]]),
               test.data=na.omit(test.br.ls[[i]]),
               per.var=per.var,
               metric=metric,
               alpha=alpha)))
    }
  }
  
  # results.br.ls <- list()
  # for(i in 1:length(train.br.ls)) {
  #   if(nrow(na.omit(train.br.ls[[i]]))<ceiling(nrow(train.data)*0.01)) {
  #       next
  #   } else {
  #     results.br.ls[[length(results.br.ls)+1]] <- applyPCA(train.data=na.omit(train.br.ls[[i]]),
  #                                                          test.data=na.omit(test.br.ls[[i]]),
  #                                                          per.var=per.var,
  #                                                          metric=metric,
  #                                                          alpha=alpha)
  #   }
  # }
  
  # For length of MT list
  results.mt.ls <- foreach(i=1:length(train.mt.ls), .combine='c', 
                           .packages = c("xts"),
                           .export = c("applyPCA")) %dopar% {
    if(nrow(na.omit(train.mt.ls[[i]]))<ceiling(nrow(train.data)*0.01)) {
      return(NA)
    } else {
      return(list(applyPCA(train.data=na.omit(train.mt.ls[[i]]),
                           test.data=na.omit(test.mt.ls[[i]]),
                           per.var=per.var,
                           metric=metric,
                           alpha=alpha)))
    }
  }
  # 
  # results.mt.ls <- list()
  # for(i in 1:length(train.mt.ls)) {
  #   if((nrow(na.omit(train.mt.ls[[i]]))<ceiling(nrow(train.data)*0.01)) || (length(na.omit(test.mt.ls[[i]]))==0)) { # If there are insufficient number of obs to train on
  #       next
  #   } else {
  #     results.mt.ls[[length(results.mt.ls)+1]] <- applyPCA(train.data=na.omit(train.mt.ls[[i]]),
  #                                                          test.data=na.omit(test.mt.ls[[i]]),
  #                                                          per.var=per.var,
  #                                                          metric=metric,
  #                                                          alpha=alpha)
  #     }
  #   }
  stopCluster(cl)
  return(list(do.call("rbind", results.br.ls[which(unlist(lapply(results.br.ls, function(x) !anyNA(x[[1]]))))]), 
              do.call("rbind", results.mt.ls[which(unlist(lapply(results.mt.ls, function(x) !anyNA(x[[1]]))))]),
              do.call("rbind", results.br.ls[which(unlist(lapply(results.br.ls, function(x) !anyNA(x[[2]]))))]), 
              do.call("rbind", results.mt.ls[which(unlist(lapply(results.mt.ls, function(x) !anyNA(x[[2]]))))])
              )) # Changed this to handle x as list with metric
}