plot.train.process <- function(allData, col.n) {
    par(mfcol=c(length(col.n),1), mar=c(3,3,2,0.5))
    for(c in col.n) {
      plot(as.zoo(allData[,c]), xlab="", ylab="", plot.type="single", pch=20, 
           type="p", main=colnames(allData)[c], ylim=c(min(allData[,c])-(max(allData[,c])-min(allData[,c]))/5,
                                                       max(allData[,c])))

      for(n in 0:as.numeric(ceiling(difftime(range(index(allData))[2],range(index(allData))[1])))) {
        if(n>0) text(x=index(allData)[1]+n*60*60*24,y=min(allData[,c]),labels = as.character(n), col="red", pos=1)
        if(n==0) text(x=index(allData)[1]+n*60*60*24,y=min(allData[,c]),labels = "*", col="red", pos=1)
      }
      rug(x = as.POSIXct(paste(unique(substr(index(allData),1,10)),"00:00:00")), ticksize = -0.05, side = 1)
    }

}