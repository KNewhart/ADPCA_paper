


# Train and test each rolling window
compiled.results.all <- array(data=NA, c(6,length(method.parameters),length(rollingWindowDays)))
colnames.compiled.results.all <- list()
# Calculate fault detection accuracy
alarm.rate.calc <- function (FaultChecks, real.alarms) {
  alarmed.t2.obs <- which(FaultChecks$Alarm == 1)
  alarmed.t2.obs <- c(alarmed.t2.obs, which(FaultChecks$Alarm == 3))
  
  nonalarmed.t2.obs <- which(FaultChecks$Alarm == 0)
  nonalarmed.t2.obs <- c(nonalarmed.t2.obs, which(FaultChecks$Alarm == 2))
  
  alarmed.spe.obs <- which(FaultChecks$Alarm == 2)
  alarmed.spe.obs <- c(alarmed.spe.obs, which(FaultChecks$Alarm == 3))
  
  nonalarmed.spe.obs <- which(FaultChecks$Alarm == 0)
  nonalarmed.spe.obs <- c(nonalarmed.spe.obs, which(FaultChecks$Alarm == 1))
  
  
  for(i in 1:length(real.alarms)){
    oc.i <- which(index(FaultChecks)  %within% real.alarms[[i]])
    # oc.obs.n <- round(as.numeric(difftime(int_end(real.alarms[[i]]),int_start(real.alarms[[i]]), units ="mins")))
    if (i == 1) {
      oc.obs <- oc.i
      # oc.total <- oc.obs.n
    } else {
      oc.obs <- c(oc.obs, oc.i)
      # oc.total <- oc.total + oc.obs.n
    }
  }
  oc.obs.index <- index(FaultChecks)[oc.obs]
  
  ic.obs <- which(!index(FaultChecks) %in% index(FaultChecks)[oc.obs])
  ic.obs.index <- index(FaultChecks)[ic.obs]
  
  # True alarm detection rate
  true.detection.t2 <- which(alarmed.t2.obs %in% oc.obs)
  tdr.t2 <- length(true.detection.t2)/length(oc.obs)
  if(is.nan(tdr.t2)) tdr.t2 <- 0
  
  true.detection.spe <- which(alarmed.spe.obs %in% oc.obs)
  tdr.spe <- length(true.detection.spe)/length(oc.obs)
  if(is.nan(tdr.spe)) tdr.spe <- 0
  
  # IC obs are alarmed "false alarms"
  alarmed.t2.ic.obs <- which(alarmed.t2.obs %in% ic.obs)
  fdr.t2 <- length(alarmed.t2.ic.obs)/length(ic.obs)
  if(is.nan(fdr.t2)) fdr.t2 <- 0
  
  alarmed.spe.ic.obs <- which(alarmed.spe.obs %in% ic.obs)
  fdr.spe <- length(alarmed.spe.ic.obs)/length(ic.obs)
  if(is.nan(fdr.spe)) fdr.spe <- 0
  
  # OC observations are not alarmed "missed alarms"
  nonalarmed.t2.oc.obs <- which(nonalarmed.t2.obs %in% oc.obs)
  ficr.t2 <- length(nonalarmed.t2.oc.obs)/length(oc.obs)
  if(is.nan(ficr.t2)) ficr.t2 <- 0
  
  nonalarmed.spe.oc.obs <- which(nonalarmed.spe.obs %in% oc.obs)
  ficr.spe <- length(nonalarmed.spe.oc.obs)/length(oc.obs)
  if(is.nan(ficr.spe)) ficr.spe <- 0
  
  return(c("TDR-T2" = tdr.t2,
           "FDR-T2" = fdr.t2, 
           "FICR-T2" = ficr.t2,
           "TDR-SPE" = tdr.spe,
           "FDR-SPE" = fdr.spe, 
           "FICR-SPE" = ficr.spe))
}
for(training.days in rollingWindowDays) {
  # Train and test each method
  print(paste("Starting", training.days, "day rolling window"))
  results.all <- vector()
  r.method <- 1
  for(r.method in 1:length(method.parameters)) {
    testing.stat <- "SPE"
    train1A_ls <- mvMonitoringv2::mspTrain(
      data = train.and.test.data[,as.numeric(unlist(method.parameters[[r.method]][2]))],
      trainObs = nrow(train.and.test.data[paste0("/",dayOne+training.days)]),
      labelVector = rep(1, nrow(rawData)), 
      updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
      Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
      faultsToTriggerAlarm = 3,
      statistic=testing.stat
    )
    alarm.results <- as.data.frame(alarm.rate.calc(train1A_ls$FaultChecks, real.faults))
    print(paste(as.character(unlist(method.parameters[[r.method]][1])), testing.stat,"completed"))
    
    testing.stat <- "T2"
    train1A_ls <- mvMonitoringv2::mspTrain(
      data = train.and.test.data[,as.numeric(unlist(method.parameters[[r.method]][2]))],
      trainObs = nrow(train.and.test.data[paste0("/",dayOne+training.days)]),
      labelVector = rep(1, nrow(rawData)), 
      updateFreq = as.numeric(unlist(method.parameters[[r.method]][3])),
      Dynamic = as.logical(unlist(method.parameters[[r.method]][4])), 
      faultsToTriggerAlarm = 3,
      statistic=testing.stat
    )
    alarm.results <- cbind(alarm.results, as.data.frame(alarm.rate.calc(train1A_ls$FaultChecks, real.faults)))
    alarm.results[c(1:3),1] <- alarm.results[c(1:3),2]
    results.all <- cbind(results.all, alarm.results[,1])
    colnames(results.all)[r.method] <- as.character(unlist(method.parameters[[r.method]][1]))
    print(paste(as.character(unlist(method.parameters[[r.method]][1])), testing.stat,"completed"))
  }
  compiled.results.all[,,which(training.days == rollingWindowDays)] <- results.all
  colnames.compiled.results.all[[which(training.days == rollingWindowDays)]] <- colnames(results.all)
}
ss_results <- compiled.results.all
ss_cols <- colnames.compiled.results.all