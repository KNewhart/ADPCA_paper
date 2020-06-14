plot.test.process.alarms <- function(allData, col.n, results.days.ls, subsys="BR", state="MS") {
  alarms.tot <- vector()
    par(mfcol=c(length(col.n),1), mar=c(2.5,2.5,2,3.5))
    for(c in col.n) {
      
      plot(as.zoo(allData[,c]), xlab="", ylab="", plot.type="single", pch=20, 
           type="p", main=colnames(allData)[c])
      mtext("Training Days", side=4, line=2.5, cex=.75)
      # points(as.zoo(allData[,c][unique(as.POSIXct(alarms.tot, origin="1970-01-01"))]), col="red")
      # for(n in 0:length(results.days.ls)) {
      #   if(n>0) text(x=index(allData)[1]+n*60*60*24,y=min(allData[,c])+.045,labels = n, col="red", pos=1)
      #   if(n==0) text(x=index(allData)[1]+n*60*60*24,y=min(allData[,c])+.045,labels = "*", col="red", pos=1)
      # }
      rug(x = as.POSIXct(paste(unique(substr(index(allData),1,10)),"00:00:00")), ticksize = -0.05, side = 1)
      
      par(new=TRUE)
      plot(as.zoo(xts(rep(-1, nrow(allData)), order.by=index(allData))), 
           ylim=c(1,14), axes=FALSE)
      axis(side=4)
      abline(h=c(5,10), lty=2)
      
      # SPE
      # MS ADPCA BR SPE
      for(day in 1:length(results.days.ls)) {
        data.ls <- results.days.ls[[day]] # 1 =One day
        n <- which(names(data.ls) == paste0(state,"ADPCA-",subsys,"-SPE"))
        alarms <- index(data.ls[[n]])[which(data.ls[[n]]=="OC")]
        alarms.diff <- vector()
        for(i in 3:length(alarms)) {
          alarms.diff <- c(alarms.diff, as.numeric(difftime(alarms[i], alarms[i-2], units = "min")))
        }
        alarms <- alarms[which(alarms.diff==2)+2]
        points(as.zoo(xts(rep(day, length(alarms)), 
                          order.by=alarms)), 
               type="p", col="pink",  cex=1.25, pch=20)
        
        alarms.tot <- c(alarms.tot, alarms)
      }
      
      # MS ADPCA BR T2
      for(day in 1:length(results.days.ls)) {
        data.ls <- results.days.ls[[day]] # 1 =One day
        n <- which(names(data.ls) == paste0(state,"ADPCA-",subsys,"-T2"))
        alarms <- index(data.ls[[n]])[which(data.ls[[n]]=="OC")]
        alarms.diff <- vector()
        for(i in 3:length(alarms)) {
          alarms.diff <- c(alarms.diff, as.numeric(difftime(alarms[i], alarms[i-2], units = "min")))
        }
        alarms <- alarms[which(alarms.diff==2)+2]
        points(as.zoo(xts(rep(day, length(alarms)), 
                          order.by=alarms)), 
               type="p", col="lightblue",  cex=1.25, pch=20)
        
        alarms.tot <- c(alarms.tot, alarms)
      }
      
      # MS ADPCA BR SPE-T2
      for(day in 1:length(results.days.ls)) {
        data.ls <- results.days.ls[[day]] # 1 =One day
        n <- which(names(data.ls) == paste0(state,"ADPCA-",subsys,"-SPE-T2"))
        alarms <- index(data.ls[[n]])[which(data.ls[[n]]=="OC")]
        alarms.diff <- vector()
        for(i in 3:length(alarms)) {
          alarms.diff <- c(alarms.diff, as.numeric(difftime(alarms[i], alarms[i-2], units = "min")))
        }
        alarms <- alarms[which(alarms.diff==2)+2]
        points(as.zoo(xts(rep(day, length(alarms)), 
                          order.by=alarms)), 
               type="p", col="purple",  cex=1.25, pch=20)
        
        alarms.tot <- c(alarms.tot, alarms)
      }
      
    }
    
    legend("bottomright", legend=c("SPE", "T2", "SPE-T2"), col=c("pink", "lightblue", "purple"),
           pch=20, pt.cex=1.25)
    
}