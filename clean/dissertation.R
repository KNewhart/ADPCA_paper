setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE), source)

##### Load raw data #####
library(xts)
if(!("rawData.RData" %in% list.files("results"))) {
  dataLocation <- "C:/Users/kbnewhart/Dropbox/Data/MP_SBMBR_data/LogAllData/"
  rawData <- loadandcleanDBF(dataLocation)
  rawData <- xts(rawData[,-1], order.by = rawData[,1])
  save(rawData, file="results/rawData.RData")
} else {
  load("results/rawData.RData")
}


##### Set up train/test datasets #####
rollingWindowDays <- seq(1,14,by=1)

timeframes <- c("2017-02-26/2017-04-03",
                "2017-05-28/2017-07-20",
                "2017-09-14/2018-02-28")

per.var <- 0.90

for(t in 1:length(timeframes)) {
# for(t in 2:length(timeframes)) {
  
  # Subset data to make more manageable
  allData <- rawData[timeframes[t]]
  # Remove dates when the system was shutdown
  allData <- allData[-unique(c(which(allData[,"MBR_1\\CURRENT_MODE"]==0),
                               which(allData[,"MBR_2\\CURRENT_MODE"]==0))),] # changed for 90% var, need to rerun for 99% var
  dates.ch <- unique(substr(index(allData),1,10))
  
  
  
  results.days.ls <- list()
  # For each rolling window...
  for(days in rollingWindowDays) {
    # Setup results lists
    control.limit.ls <- list()
    control.chart.ls <- list()
    sspca.spe.ls <- list()
    sspca.t2.ls <- list()
    sspca.both.ls <- list()
    mspca.spe.ls <- list()
    mspca.t2.ls <- list()
    mspca.both.ls <- list()
    
    # Loop over every day in dataset...
    for(date.n in 1:(length(dates.ch)-days)) {
      
      # 1. U/LCL
      date <- as.Date(dates.ch[date.n])
      train <- allData[paste(date, date+days-1, sep="/")]
      test <- allData[paste(date+days)]
      control.limit.ls[[length(control.limit.ls)+1]] <- control.limits(test.data=test)
      
      # 2. Control charts
      # date <- as.Date(dates.ch[date.n])
      # train <- allData[paste(date, date+days-1, sep="/")]
      # test <- allData[paste(date+days)]
      # if(length(control.chart.ls)>0) { # Exclude OC obs from previous testing data
      #   obs.n <- which(control.chart.ls[[length(control.chart.ls)]][,1]=="OC")
      #   if(length(obs.n)>0) obs.t <- index(control.chart.ls[[length(control.chart.ls)]])[-obs.n]
      #   if(length(obs.n)==0) obs.t <- index(train)
      # } else {
      #   obs.t <- index(train)
      # }
      # control.chart.ls[[length(control.chart.ls)+1]] <- control.chart(train.data=train[obs.t],
      #                                                                 test.data=test,
      #                                                                 limits=5)
      
      # 3. SS PCA
      date <- as.Date(dates.ch[date.n])
      train <- allData[paste(date, date+days-1, sep="/")]
      test <- allData[paste(date+days)]
      
      if(length(sspca.spe.ls)>0) { # Exclude OC obs from previous testing data
        obs.n <- which(sspca.spe.ls[[length(sspca.spe.ls)]]=="OC")
        if(length(obs.n)>0) obs.t <- index(sspca.spe.ls[[length(sspca.spe.ls)]])[-obs.n]
        if(length(obs.n)==0) obs.t <- index(train)
      } else {
        obs.t <- index(train)
      }
      sspca.spe.ls[[length(sspca.spe.ls)+1]] <- adpca(train.data=train[obs.t],
                                                      test.data=test,
                                                      dynamic=TRUE,
                                                      multistate=FALSE,
                                                      alpha=0.01,
                                                      per.var=per.var,
                                                      metric="SPE")
      
      if(length(sspca.t2.ls)>0) { # Exclude OC obs from previous testing data
        obs.n <- which(sspca.t2.ls[[length(sspca.t2.ls)]]=="OC")
        if(length(obs.n)>0) obs.t <- index(sspca.t2.ls[[length(sspca.t2.ls)]])[-obs.n]
        if(length(obs.n)==0) obs.t <- index(train)
      } else {
        obs.t <- index(train)
      }
      sspca.t2.ls[[length(sspca.t2.ls)+1]] <- adpca(train.data=train,
                                                    test.data=test,
                                                    dynamic=TRUE,
                                                    multistate=FALSE,
                                                    alpha=0.01,
                                                    per.var=per.var,
                                                    metric="T2")
      
      if(length(sspca.both.ls)>0) { # Exclude OC obs from previous testing data
        obs.n <- which(sspca.both.ls[[length(sspca.both.ls)]]=="OC")
        if(length(obs.n)>0) obs.t <- index(sspca.both.ls[[length(sspca.both.ls)]])[-obs.n]
        if(length(obs.n)==0) obs.t <- index(train)
      } else {
        obs.t <- index(train)
      }
      sspca.both.ls[[length(sspca.both.ls)+1]] <- adpca(train.data=train,
                                                        test.data=test,
                                                        dynamic=TRUE,
                                                        multistate=FALSE,
                                                        alpha=0.01,
                                                        per.var=per.var,
                                                        metric="SPE-T2")
      
      # 4. MS ADPCA
      date <- as.Date(dates.ch[date.n])
      train <- allData[paste(date, date+days-1, sep="/")]
      test <- allData[paste(date+days)]
      
      if(length(mspca.spe.ls)>0) { # Exclude OC obs from previous testing data
        obs.n <- which(mspca.spe.ls[[length(mspca.spe.ls)]]=="OC")
        if(length(obs.n)>0) obs.t <- index(mspca.spe.ls[[length(mspca.spe.ls)]])[-obs.n]
        if(length(obs.n)==0) obs.t <- index(train)
      } else {
        obs.t <- index(train)
      }
      mspca.spe.ls[[length(mspca.spe.ls)+1]] <- adpca(train.data=train[obs.t],
                                                      test.data=test,
                                                      dynamic=TRUE,
                                                      multistate=TRUE,
                                                      alpha=0.01,
                                                      per.var=per.var,
                                                      metric="SPE")
      
      if(length(mspca.t2.ls)>0) { # Exclude OC obs from previous testing data
        obs.n <- which(mspca.t2.ls[[length(mspca.t2.ls)]]=="OC")
        if(length(obs.n)>0) obs.t <- index(mspca.t2.ls[[length(mspca.t2.ls)]])[-obs.n]
        if(length(obs.n)==0) obs.t <- index(train)
      } else {
        obs.t <- index(train)
      }
      mspca.t2.ls[[length(mspca.t2.ls)+1]] <- adpca(train.data=train,
                                                    test.data=test,
                                                    dynamic=TRUE,
                                                    multistate=TRUE,
                                                    alpha=0.01,
                                                    per.var=per.var,
                                                    metric="T2")
      
      if(length(mspca.both.ls)>0) { # Exclude OC obs from previous testing data
        obs.n <- which(mspca.both.ls[[length(mspca.both.ls)]]=="OC")
        if(length(obs.n)>0) obs.t <- index(mspca.both.ls[[length(mspca.both.ls)]])[-obs.n]
        if(length(obs.n)==0) obs.t <- index(train)
      } else {
        obs.t <- index(train)
      }
      mspca.both.ls[[length(mspca.both.ls)+1]] <- adpca(train.data=train,
                                                        test.data=test,
                                                        dynamic=TRUE,
                                                        multistate=TRUE,
                                                        alpha=0.01,
                                                        per.var=per.var,
                                                        metric="SPE-T2")
    }
    # Combine results for all days tested
    results.days.ls[[length(results.days.ls)+1]] <- list("Persistence"=do.call("rbind", control.limit.ls), # Univariate static control limits
                                                         "SSPCA-BR-SPE"=do.call("rbind", lapply(sspca.spe.ls, function(x) x[[1]])), # SS PCA for BR
                                                         "SSPCA-MT-SPE"=do.call("rbind", lapply(sspca.spe.ls, function(x) x[[2]])), # SS PCA for MT
                                                         "SSPCA-BR-T2"=do.call("rbind", lapply(sspca.t2.ls, function(x) x[[1]])), # SS PCA for BR
                                                         "SSPCA-MT-T2"=do.call("rbind", lapply(sspca.t2.ls, function(x) x[[2]])), # SS PCA for MT
                                                         "SSPCA-BR-SPE-T2"=do.call("rbind", lapply(sspca.both.ls, function(x) x[[1]])), # SS PCA for BR
                                                         "SSPCA-MT-SPE-T2"=do.call("rbind", lapply(sspca.both.ls, function(x) x[[2]])), # SS PCA for MT
                                                         "MSADPCA-BR-SPE"=do.call("rbind", lapply(mspca.spe.ls, function(x) x[[1]])), # MS AD-PCA for BR
                                                         "MSADPCA-MT-SPE"=do.call("rbind", lapply(mspca.spe.ls, function(x) x[[2]])), # MS AD-PCA for MT
                                                         "MSADPCA-BR-T2"=do.call("rbind", lapply(mspca.t2.ls, function(x) x[[1]])), # MS AD-PCA for BR
                                                         "MSADPCA-MT-T2"=do.call("rbind", lapply(mspca.t2.ls, function(x) x[[2]])), # MS AD-PCA for MT
                                                         "MSADPCA-BR-SPE-T2"=do.call("rbind", lapply(mspca.both.ls, function(x) x[[1]])), # MS AD-PCA for BR
                                                         "MSADPCA-MT-SPE-T2"=do.call("rbind", lapply(mspca.both.ls, function(x) x[[2]])) # MS AD-PCA for MT
    )
  }
  names(results.days.ls) <- paste(rollingWindowDays,"Days")
  
  save(results.days.ls, file=paste0("results/results-days-ls-",gsub("/"," ",timeframes[t])," ",round(per.var*100,0),"percent.RData"))
  
}


