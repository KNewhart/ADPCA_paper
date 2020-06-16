plot.test.process.alarms <- function(allData, col.n, results.days.ls, subsys="BR", state="MS", multiple=TRUE, mfcol.c = c(length(col.n),1), legend.l=TRUE) {
  alarms.tot <- vector()
  colors.n <- c("#D81B60", "#1E88E5", "#D47D1B")
    if(multiple) par(mfcol=mfcol.c, mar=c(2.5,2.5,2,3.5))
  if(!multiple) par(mar=c(2.5,3.5,2,3.5))
    for(c in col.n) {
      
      if(multiple) plot(as.zoo(allData[,c]), xlab="", ylab="", plot.type="single", pch=20, 
           type="p", main=colnames(allData)[c])
      if(!multiple) plot(as.zoo(allData[,c]), xlab="", plot.type="single", pch=20, 
                        type="p", ylab="")
      if(!multiple) mtext(colnames(allData)[c], side=2, line=2.5, cex=.75)
      mtext("Training Days", side=4, line=2.5, cex=.75)
      # points(as.zoo(allData[,c][unique(as.POSIXct(alarms.tot, origin="1970-01-01"))]), col="red")
      # for(n in 0:length(results.days.ls)) {
      #   if(n>0) text(x=index(allData)[1]+n*60*60*24,y=min(allData[,c])+.045,labels = n, col="red", pos=1)
      #   if(n==0) text(x=index(allData)[1]+n*60*60*24,y=min(allData[,c])+.045,labels = "*", col="red", pos=1)
      # }
      rug(x = as.POSIXct(paste(unique(substr(index(allData),1,10)),"00:00:00")), ticksize = -0.05, side = 1)
      
      par(new=TRUE)
      plot(as.zoo(xts(rep(-1, nrow(allData)), order.by=index(allData))), 
           ylim=c(1,14), axes=FALSE, xlab="", ylab="")
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
        alarms <- xts(rep(day, length(alarms)), 
                      order.by=alarms)[paste(range(index(allData)), collapse ="/")]
        points(as.zoo(alarms), 
               type="p", col=colors.n[1],  cex=1.25, pch=20)
        
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
        alarms <- xts(rep(day, length(alarms)), 
                      order.by=alarms)[paste(range(index(allData)), collapse ="/")]
        points(as.zoo(alarms),  
               type="p", col=colors.n[2],  cex=1.25, pch=20)
        
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
        alarms <- xts(rep(day, length(alarms)), 
                      order.by=alarms)[paste(range(index(allData)), collapse ="/")]
        points(as.zoo(alarms), 
               type="p", col=colors.n[3],  cex=1.25, pch=20)
        
        alarms.tot <- c(alarms.tot, alarms)
      }
      
    }
    
    if(legend.l) legend("bottomright", legend=c("SPE", "T2", "SPE-T2"), col=c(colors.n[1], colors.n[2], colors.n[3]),
           pch=20, pt.cex=1.25)
    
}