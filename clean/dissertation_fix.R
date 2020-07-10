setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE, pattern = ".R"), source)

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
                "2017-09-14/2018-02-28",
                "2018-05-01/2018-09-05",
                "2019-07-01/2020-01-13"
)

for(per.var in c(0.99, 0.9, 0.8)) {
  for(alpha in c(0.01, 0.10)) {
    for(t in 1:length(timeframes)) {
      # Subset data to make more manageable
      allData <- rawData[timeframes[t]]
      # Remove dates when the system was shutdown
      allData <- allData[-unique(c(which(allData[,"MBR_1\\CURRENT_MODE"]==0),
                                   which(allData[,"MBR_2\\CURRENT_MODE"]==0))),] 
      dates.ch <- unique(substr(index(allData),1,10))
      
      
      metrics.days.ls <- list()
      results.days.ls <- list()
      
      # For each rolling window...
      for(days in rollingWindowDays) {
        # Setup results lists
        sspca.both.ls <- list()
        mspca.both.ls <- list()
        
        # Loop over every day in dataset...
        for(date.n in 1:(length(dates.ch)-days)) {
          
          # 3. SS PCA
          date <- as.Date(dates.ch[date.n])
          train <- allData[paste(date, date+days-1, sep="/")]
          test <- allData[paste(date+days)]
          
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
                                                            alpha=alpha,
                                                            per.var=per.var,
                                                            metric="SPE-T2")
          
          # 4. MS ADPCA
          date <- as.Date(dates.ch[date.n])
          train <- allData[paste(date, date+days-1, sep="/")]
          test <- allData[paste(date+days)]
          
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
                                                            alpha=alpha,
                                                            per.var=per.var,
                                                            metric="SPE-T2")
        }
        print(paste(Sys.time(),"Training Window",days,"Completed."))

        # Combine results for all days tested
        results.days.ls[[length(results.days.ls)+1]] <- list("SSPCA-BR-SPE-T2"=do.call("rbind", lapply(sspca.both.ls, function(x) x[[1]])), # SS PCA for BR
                                                             "SSPCA-MT-SPE-T2"=do.call("rbind", lapply(sspca.both.ls, function(x) x[[2]])), # SS PCA for MT
                                                             "MSADPCA-BR-SPE-T2"=do.call("rbind", lapply(mspca.both.ls, function(x) x[[1]])), # MS AD-PCA for BR
                                                             "MSADPCA-MT-SPE-T2"=do.call("rbind", lapply(mspca.both.ls, function(x) x[[2]])) # MS AD-PCA for MT
        )
        metrics.days.ls[[length(metrics.days.ls)+1]] <- list("SSPCA-BR-SPE-T2"=do.call("rbind", lapply(sspca.both.ls, function(x) x[[3]])), # SS PCA for BR
                                                             "SSPCA-MT-SPE-T2"=do.call("rbind", lapply(sspca.both.ls, function(x) x[[4]])), # SS PCA for MT
                                                             "MSADPCA-BR-SPE-T2"=do.call("rbind", lapply(mspca.both.ls, function(x) x[[3]])), # MS AD-PCA for BR
                                                             "MSADPCA-MT-SPE-T2"=do.call("rbind", lapply(mspca.both.ls, function(x) x[[4]])) # MS AD-PCA for MT
        )
      }
      names(results.days.ls) <- paste(rollingWindowDays,"Days")
      names(metrics.days.ls) <- paste(rollingWindowDays,"Days")
      
      save(results.days.ls, file=paste0("results/results-days-ls-fix-",gsub("/"," ",timeframes[t])," ",round(per.var*100,0),"percent ",round(alpha*100,0),"alpha.RData"))
      save(metrics.days.ls, file=paste0("results/metrics-days-ls-fix-",gsub("/"," ",timeframes[t])," ",round(per.var*100,0),"percent ",round(alpha*100,0),"alpha.RData"))
    }
    
  }
  
}
