plot.test.per.thresh <- function(metric.ls, date.range=NULL, metric.n, y.lim=100, type="p", n=1) {
  library(xts)
  colors.n <- c("#D81B60", "#1E88E5", "#D47D1B")
  metric.xts <- metric.ls[[metric.n]]
  if(!is.null(date.range)) metric.xts <- metric.xts[date.range]
  
  if(any(grep("SPE-T2", names(metric.ls)[metric.n]))) {
    metric <- "SPE-T2"
  } else {
    if(any(grep("SPE", names(metric.ls)[metric.n]))) metric <- "SPE"
    if(any(grep("T2", names(metric.ls)[metric.n]))) metric <- "T2"
  }
  
  if(metric=="SPE") {
    per.of.thresh <- metric.xts[,1]/metric.xts[,2]*100
    if(any(per.of.thresh>y.lim)) ooo <- which(per.of.thresh > y.lim)
    if(any(per.of.thresh>100)) alarm <- which(per.of.thresh > 100)
    # if(type=="p") plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of SPE threshold", 
    #      col=colors.n[1], ylim=c(0,y.lim))
    # if(type=="l") plot(as.zoo(per.of.thresh), type="l", xlab="", ylab="Percent of SPE threshold", 
         # col=colors.n[1], ylim=c(0,y.lim))
    if(type=="p") plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of SPE threshold", 
                       col=colors.n[1], log="y", ylim=c(0.01,105), yaxt="n", format='%b-%d', cex=n)
    if(type=="l") plot(as.zoo(per.of.thresh), type="l", xlab="", ylab="Percent of SPE threshold", 
         col=colors.n[1], log="y", ylim=c(0.01,105), yaxt="n", format='%b-%d')
    y1 <- floor(log10(range(per.of.thresh)))
    pow <- seq(y1[1], y1[2]+1)
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, 10^pow, labels = as.character(10^pow))
    axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
    # if(any(per.of.thresh>y.lim)) points(as.zoo(xts(rep(y.lim,length(ooo)), order.by=index(per.of.thresh)[ooo])), pch=17,col=colors.n[1])
    if(any(per.of.thresh>100)) points(as.zoo(xts(rep(100,length(alarm)), order.by=index(per.of.thresh)[alarm])), pch=17, cex=n)
  }
  
  if(metric=="T2") {
    per.of.thresh <- metric.xts[,3]/metric.xts[,4]*100
    if(any(per.of.thresh>y.lim)) ooo <- which(per.of.thresh > y.lim)
    if(any(per.of.thresh>100)) alarm <- which(per.of.thresh > 100)
    if(type=="p") plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of T2 threshold", 
                       col=colors.n[2], log="y", ylim=c(0.01,105), yaxt="n", format='%b-%d', cex=n)
    if(type=="l") plot(as.zoo(per.of.thresh), type="l", xlab="", ylab="Percent of T2 threshold", 
                       col=colors.n[2], log="y", ylim=c(0.01,105), yaxt="n", format='%b-%d')
    y1 <- floor(log10(range(per.of.thresh)))
    pow <- seq(y1[1], y1[2]+1)
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, 10^pow, labels = as.character(10^pow))
    axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
    # plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of T2 threshold", 
         # col=colors.n[2], ylim=c(0,y.lim))
    # if(any(per.of.thresh>y.lim)) points(as.zoo(xts(rep(y.lim,length(ooo)), order.by=index(per.of.thresh)[ooo])), pch=17,col=colors.n[2])
    if(any(per.of.thresh>100)) points(as.zoo(xts(rep(100,length(alarm)), order.by=index(per.of.thresh)[alarm])), pch=17, cex=n)
  }
  
  if(metric=="SPE-T2") {
    per.of.thresh <- sapply(1:nrow(metric.xts), function(i) mean(metric.xts[i,1]/metric.xts[i,2]*100,metric.xts[i,3]/metric.xts[i,4]*100))
    if(any(per.of.thresh>y.lim)) ooo <- which(per.of.thresh > y.lim)
    if(any(per.of.thresh>100)) alarm <- which(per.of.thresh > 100)
    per.of.thresh <- xts(per.of.thresh, order.by=index(metric.xts))
    if(type=="p") plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of SPE-T2 threshold", 
                       col=colors.n[3], log="y", ylim=c(0.01,105), yaxt="n", format='%b-%d', cex=n)
    if(type=="l") plot(as.zoo(per.of.thresh), type="l", xlab="", ylab="Percent of SPE-T2 threshold", 
                       col=colors.n[3], log="y", ylim=c(0.01,105), yaxt="n", format='%b-%d')
    y1 <- floor(log10(range(per.of.thresh)))
    pow <- seq(y1[1], y1[2]+1)
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, 10^pow, labels = as.character(10^pow))
    axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
    # plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of SPE-T2 threshold", 
    #      col=colors.n[3], ylim=c(0,y.lim))
    # if(any(per.of.thresh>y.lim)) points(as.zoo(xts(rep(y.lim,length(ooo)), order.by=index(per.of.thresh)[ooo])), pch=17,col=colors.n[3])
    if(any(per.of.thresh>100)) points(as.zoo(xts(rep(100,length(alarm)), order.by=index(per.of.thresh)[alarm])), pch=17, cex=n)
  }
  
  
}
