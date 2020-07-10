zoom.in <- function(results.days.ls, plotdata, col.n, subsys, state, per.var, alpha) {
  sys.n <- intersect(grep(state, names(results.days.ls[[1]])), grep(subsys, names(results.days.ls[[1]])))
  dates <- substr(na.omit(unlist(lapply(results.days.ls[length(results.days.ls)], 
                                       function(days.ls) lapply(days.ls[sys.n], 
                                                                function(test) as.character(index(test)[which(test == "OC")]))))),1,10)
  dates <- unique(dates)[which(sapply(unique(dates), function(x) length(which(dates==x)))>3)]
  for(date in dates) {
    m <- which(substr(as.character(index(plotdata)),1,10) == date)
    png(filename=paste0(date," ", per.var," ", alpha*100," ",state," ",subsys,".png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
    plot.test.process.alarms(allData=plotdata[m,], 
                             col.n=col.n, results.days.ls=results.days.ls, 
                             subsys = subsys, state = state)
    dev.off()  
    print(paste("Completed", date))
  }
}
