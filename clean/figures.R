library(xts)
setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE, pattern=".R"), source)

###### Figure 3.3, 3.5 #####
plot.test.process.alarms.v2 <- function(allData, 
                                        col.n=1:ncol(allData), 
                                        results.days.ls,
                                        subsys="BR", 
                                        state="MS", 
                                        legend.l=TRUE,
                                        y.axt.v=NULL, 
                                        main.v=colnames(allData)[col.n],
                                        per.var,
                                        alpha,
                                        minor.ticks=TRUE,
                                        date.format="%b-%d %H:%M") {
  library(scales)
  
  alarms.tot <- vector()
  colors.n <- c("#D81B60", "#1E88E5", "#D47D1B")
  
  mfcol.c <- c(length(col.n)*2,1) 
  par(mfcol=mfcol.c, mar=c(2.5,4.5,2.5,2))
  
  for(c in col.n) {
    plot(as.zoo(allData[,c]), 
         xlab="", ylab="", 
         plot.type="single", 
         pch=20, type="l", 
         xaxt="n")
    mtext(main.v[which(c==col.n)], 
          cex=par()$cex.main, side=3, line=.5, font=2)
    axis.dates <- c(do.call("c",
                            list(sapply(unique(substr(index(allData),1,10)), 
                                        function(x) paste(x, c("00:00:00","12:00:00"))))),
                    paste(as.Date(tail(index(allData),n=1)),"00:00:00"))
    axis.POSIXct(side=1,
                 x=index(allData),
                 at=axis.dates,
                 format=date.format)
    mtext(y.axt.v[which(c==col.n)], side=2, line=2.5, cex=par()$cex.main)
    if(minor.ticks) rug(x = as.POSIXct(paste0(unique(substr(index(allData),1,13)),":00:00")), ticksize = -0.05, side = 1)
    
    # par(new=TRUE)
    plot(as.zoo(xts(rep(-1, nrow(allData)), order.by=index(allData))), 
         ylim=c(1,14), axes=FALSE, xlab="", ylab="")
    mtext(paste0(state,"AD-PCA, ",per.var,"% variance, ",alpha*100,"% signifiance"), 
          cex=par()$cex.main, side=3, line=1.25, font=2)
    axis(side=2)
    axis.POSIXct(side=1,
                 x=index(allData),
                 at=axis.dates,
                 format=date.format)
    abline(h=c(5,10), lty=2)
    mtext("Training Days", side=2, line=2.5, cex=par()$cex.main)
    if(minor.ticks) rug(x = as.POSIXct(paste0(unique(substr(index(allData),1,13)),":00:00")), ticksize = -0.05, side = 1)
    
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
             type="p", col=alpha(colors.n[1],.5),  cex=1.25, pch=20)
      
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
             type="p", col=alpha(colors.n[2],.5),  cex=1.25, pch=20)
      
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
             type="p", col=alpha(colors.n[3],.5),  cex=1.25, pch=20)
      
      alarms.tot <- c(alarms.tot, alarms)
    }
    
  }
  legend.text <- c(expression('SPE'^""), expression('T'^"2"), expression('SPE-T'^"2"))
  
  if(legend.l) legend("top",
                      legend=legend.text,
                      col=c(colors.n[1], colors.n[2], colors.n[3]),
                      pch=20,
                      pt.cex=1.25,
                      horiz = TRUE,
                      xpd=TRUE,
                      inset=c(0,-.225),
                      bty ="n")
}


load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]
alpha <- 0.1
per.var <- 99
filename <- paste0("results/results-days-ls-2017-02-26 2017-04-03 ",per.var,"percent ",alpha*100,"alpha.RData")
if(!(filename %in% list.files("results", full.names = TRUE))) next
load(filename)
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData["2017-03-21"]
col.n <- c(which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE"))
# col.n <- which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-13-a.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="SS",
                            y.axt.v=c("PSI"), 
                            main.v=c("MBR 1 Transmembrane Pressure"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=TRUE)
dev.off()
col.n <- c(which(colnames(allData)=="PERMEATE_TANK\\CONDUCTIVITY\\PROCESS_VALUE"))
# col.n <- which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-13-b.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="SS",
                            y.axt.v=c("uS/cm"), 
                            main.v=c("Permeate Tank Conductivity"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=TRUE)
dev.off()






allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData["2017-03-13/2017-03-14"]
col.n <- which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE")

png(filename=paste0("dissertation_figures/final/Figure 3-3.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="MS",
                            y.axt.v=c("PSI"), 
                            main.v=c("MBR 1 Transmembrane Pressure"),
                            per.var=per.var,
                            alpha=alpha)
dev.off()


col.n <- which(colnames(allData)=="BIO_2\\TSS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-3-b.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="SS",
                            y.axt.v=c("TSS"), 
                            main.v=c("MBR 2 Total Suspended Solids"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=FALSE)
dev.off()

# Figure 3.5
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData["2017-03-13/2017-04-03"]
col.n <- which(colnames(allData)=="PERMEATE_TANK\\CONDUCTIVITY\\PROCESS_VALUE")
# col.n <- which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-5-a.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="SS",
                            y.axt.v=c("uS/cm"), 
                            main.v=c("Permeate Tank Conductivity"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=FALSE)
dev.off()

# col.n <- which(colnames(allData)=="PERMEATE_TANK\\CONDUCTIVITY\\PROCESS_VALUE")
col.n <- which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-5-b.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="MS",
                            y.axt.v=c("PSI"), 
                            main.v=c("MBR 1 Transmembrane Pressure"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=FALSE)
dev.off()


per.var <- 90
alpha <-.10
filename <- paste0("results/results-days-ls-2017-02-26 2017-04-03 ",per.var,"percent ",alpha*100,"alpha.RData")
load(filename)
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData["2017-03-28 00:00:00/2017-03-29 08:00:00"]
col.n <- which(colnames(allData)=="PERMEATE_TANK\\CONDUCTIVITY\\PROCESS_VALUE")
# col.n <- which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-8.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="MS",
                            y.axt.v=c("uS/cm"), 
                            main.v=c("Permeate Tank Conductivity"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=TRUE)
dev.off()


metrics.days.date <- "2017-09-14 2018-02-28"
per.var <- 99
alpha <-.01
filename <- paste0("results/results-days-ls-",metrics.days.date," ",per.var,"percent ",alpha*100,"alpha.RData")
load(filename)
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}

allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData["2017-11-30 00:00:00/2017-12-01 08:00:00"]
col.n <- which(colnames(allData)== "RAS_TROUGH\\TSS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-10.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="BR",
                            state="MS",
                            y.axt.v=c("mg/L"), 
                            main.v=c("RAS Total Suspended Solids"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=TRUE)
dev.off()


allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData["2017-10-05"]
col.n <- which(colnames(allData)== "BIO_1\\TSS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-17.png"), 
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="MS",
                            y.axt.v=c("mg/L"), 
                            main.v=c("MBR 1 Total Suspended Solids"),
                            per.var=per.var,
                            alpha=alpha,
                            minor.ticks=TRUE)
dev.off()


# Figure 3.
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]
alpha <- 0.1
per.var <- 99
metrics.days.date <- "2018-05-01 2018-09-05"
dates <- "2018-08-04 00:00:00/"
filename <- paste0("results/results-days-ls-",metrics.days.date," ",per.var,"percent ",alpha*100,"alpha.RData")
load(filename)
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData[dates]

col.n <- which(colnames(allData)=="MBR_1\\TRANS_PRESS\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-6-a.png"),
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="SS",
                            y.axt.v=c("PSI"), 
                            main.v=c("MBR 1 Transmembrane Pressure"),
                            per.var=per.var,
                            alpha=alpha, 
                            minor.ticks = FALSE,
                            date.format="%b-%d")
dev.off()

col.n <- which(colnames(allData)=="PERMEATE_TANK\\TURBIDITY\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-6-b.png"),
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="MS",
                            y.axt.v=c("NTU"), 
                            main.v=c("Permeate turbdity"),
                            per.var=per.var,
                            alpha=alpha, 
                            minor.ticks = FALSE,
                            date.format="%b-%d")
dev.off()

col.n <- which(colnames(allData)=="BIO_2\\TSS\\PROCESS_VALUE" )
png(filename=paste0("dissertation_figures/final/Figure 3-6-c.png"),
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="MT",
                            state="MS",
                            y.axt.v=c("mg/L TSS"), 
                            main.v=c("MBR 2 Total Suspended Solids"),
                            per.var=per.var,
                            alpha=alpha, 
                            minor.ticks = FALSE,
                            date.format="%b-%d")
dev.off()




load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]
alpha <- 0.1
per.var <- 99
metrics.days.date <- "2018-05-01 2018-09-05"
dates <- "2018-08-14"
filename <- paste0("results/results-days-ls-",metrics.days.date," ",per.var,"percent ",alpha*100,"alpha.RData")
load(filename)
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData[dates]


col.n <- which(colnames(allData)=="RAS_TROUGH\\TSS\\PROCESS_VALUE" )
png(filename=paste0("dissertation_figures/final/Figure 3-12-a.png"),
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="BR",
                            state="SS",
                            y.axt.v=c("mg/L"), 
                            main.v=c("RAS Total Suspended Solids"),
                            per.var=per.var,
                            alpha=alpha, 
                            minor.ticks = FALSE,
                            date.format="%H:%M")
dev.off()

col.n <- which(colnames(allData)=="BIO_1\\TEMPERATURE\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-12-b.png"),
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="BR",
                            state="SS",
                            y.axt.v=c("Fahrenheit"), 
                            main.v=c("SBR 1 Temperature"),
                            per.var=per.var,
                            alpha=alpha, 
                            minor.ticks = FALSE,
                            date.format="%H:%M")
dev.off()


col.n <- which(colnames(allData)=="BIO_2\\DO\\PROCESS_VALUE")
png(filename=paste0("dissertation_figures/final/Figure 3-12-c.png"),
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="BR",
                            state="MS",
                            y.axt.v=c("mg/L"), 
                            main.v=c("SBR 2 Dissolved Oxygen"),
                            per.var=per.var,
                            alpha=alpha, 
                            minor.ticks = FALSE,
                            date.format="%H:%M")
dev.off()





# Figure 3.15
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]
alpha <- 0.1
per.var <- 99
# metrics.days.date <- "2017-05-28 2017-07-20"
metrics.days.date <- "2017-02-26 2017-04-03"
# dates <- "2017-06-10/2017-06-17"
dates <- "2017-03-26"
filename <- paste0("results/results-days-ls-",metrics.days.date," ",per.var,"percent ",alpha*100,"alpha.RData")
load(filename)
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
allData <- allData[dates]


col.n <- which(colnames(allData)=="SEWAGE\\FLOW\\TOTAL_BATCH" )
png(filename=paste0("dissertation_figures/final/Figure 3-15.png"),
    width=10, height =5.1*length(col.n), units="in", res=900, family="serif")
plot.test.process.alarms.v2(allData,
                            col.n, 
                            results.days.ls,
                            subsys="BR",
                            state="MS",
                            y.axt.v=c("mg/L"), 
                            main.v=c("SBR Total Batch Size"),
                            per.var=per.var,
                            alpha=alpha, 
                            minor.ticks = TRUE,
                            date.format="%H:%M")
dev.off()





















##### Figure 3.4 #####
plot.test.per.thresh.v2 <- function(metric.ls, 
                                    date.range=NULL, 
                                    metric.n, 
                                    y.lim=100, 
                                    type="p", 
                                    n=1, date.format='%b-%d', 
                                    minor.ticks=TRUE,
                                    true.alarm=NULL) {
  library(xts)
  library(scales)
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

    if(type=="p") plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="", 
                       col=colors.n[1], log="y", ylim=c(0.01,105), yaxt="n", format=date.format, cex=n)
    if(type=="l") plot(as.zoo(per.of.thresh), type="l", xlab="", ylab="", 
                       col=colors.n[1], log="y", ylim=c(0.01,105), yaxt="n", format=date.format)
    y1 <- floor(log10(range(per.of.thresh)))
    pow <- seq(y1[1], y1[2]+1)
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, 10^pow, labels = as.character(10^pow))
    axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
    if(any(per.of.thresh>100)) points(as.zoo(xts(rep(100,length(alarm)), 
                                                 order.by=index(per.of.thresh)[alarm])), 
                                      pch=17, cex=n, col=alpha("black", 0.33))
  }
  
  if(metric=="T2") {
    per.of.thresh <- metric.xts[,3]/metric.xts[,4]*100
    if(any(per.of.thresh>y.lim)) ooo <- which(per.of.thresh > y.lim)
    if(any(per.of.thresh>100)) alarm <- which(per.of.thresh > 100)
    if(type=="p") plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="", 
                       col=colors.n[2], log="y", ylim=c(0.01,105), yaxt="n", format=date.format, cex=n)
    if(type=="l") plot(as.zoo(per.of.thresh), type="l", xlab="", ylab="", 
                       col=colors.n[2], log="y", ylim=c(0.01,105), yaxt="n", format=date.format)
    y1 <- floor(log10(range(per.of.thresh)))
    pow <- seq(y1[1], y1[2]+1)
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, 10^pow, labels = as.character(10^pow))
    axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
    # plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of T2 threshold", 
    # col=colors.n[2], ylim=c(0,y.lim))
    # if(any(per.of.thresh>y.lim)) points(as.zoo(xts(rep(y.lim,length(ooo)), order.by=index(per.of.thresh)[ooo])), pch=17,col=colors.n[2])
    if(any(per.of.thresh>100)) points(as.zoo(xts(rep(100,length(alarm)), 
                                                 order.by=index(per.of.thresh)[alarm])), 
                                      pch=17, cex=n, col=alpha("black", 0.33))
  }
  
  if(metric=="SPE-T2") {
    per.of.thresh <- sapply(1:nrow(metric.xts), function(i) mean(metric.xts[i,1]/metric.xts[i,2]*100,metric.xts[i,3]/metric.xts[i,4]*100))
    if(any(per.of.thresh>y.lim)) ooo <- which(per.of.thresh > y.lim)
    if(any(per.of.thresh>100)) alarm <- which(per.of.thresh > 100)
    per.of.thresh <- xts(per.of.thresh, order.by=index(metric.xts))
    if(type=="p") plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="", 
                       col=colors.n[3], log="y", ylim=c(0.01,105), yaxt="n", format=date.format, cex=n)
    if(type=="l") plot(as.zoo(per.of.thresh), type="l", xlab="", ylab="", 
                       col=colors.n[3], log="y", ylim=c(0.01,105), yaxt="n", format=date.format)
    y1 <- floor(log10(range(per.of.thresh)))
    pow <- seq(y1[1], y1[2]+1)
    ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
    axis(2, 10^pow, labels = as.character(10^pow))
    axis(2, ticksat, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
    # plot(as.zoo(per.of.thresh), type="p", pch=20, xlab="", ylab="Percent of SPE-T2 threshold", 
    #      col=colors.n[3], ylim=c(0,y.lim))
    # if(any(per.of.thresh>y.lim)) points(as.zoo(xts(rep(y.lim,length(ooo)), order.by=index(per.of.thresh)[ooo])), pch=17,col=colors.n[3])
    if(any(per.of.thresh>100)) points(as.zoo(xts(rep(100,length(alarm)), 
                                                 order.by=index(per.of.thresh)[alarm])), 
                                      pch=17, cex=n, col=alpha("black", 0.33))
    }
  
  if(minor.ticks) rug(x = as.POSIXct(paste0(unique(substr(index(metric.xts),1,13)),
                                            ":00:00")), 
                      ticksize = -0.03, side = 1)
  if(!is.null(true.alarm)) {
    tdr <- length(which(index(per.of.thresh[true.alarm]) %in% index(per.of.thresh)[alarm]))/length(index(per.of.thresh[true.alarm]))
    mtext(paste0("TDR = ",round(tdr,3)), side=3, adj=0.5, line=0, cex=1/1.2*.8)
    return(tdr)
  }
  
}

metrics.days.date <- "2018-05-01 2018-09-05"
# dates <- "2018-08-04 00:00:00/"
dates <- "2018-08-14"
# metrics.days.date <- "2017-02-26 2017-04-03"
# metrics.days.date <- "2017-09-14 2018-02-28"
# dates <- "2017-11-30/2017-12-01 08:00:00"
# dates <- "2017-03-14 00:00:00/2017-03-15 08:00:00"
# dates <- "2017-03-28 00:00:00/2017-03-29 08:00:00"
tdr.all <- matrix(data=NA, nrow=14*3*2, ncol=4)
for(subsys in c("BR")) {
  tdr.i <- 1
  for(per.var in c(99,90,80)) {
    for(alpha in c(0.01, 0.1)) {
      for(days in 1:14) {
      
        
        filename <- paste0("results/metrics-days-ls-",metrics.days.date," ",
                           per.var,"percent ",alpha*100,"alpha.RData")
        if(filename %in% list.files(recursive = TRUE)) {
          load(filename)
          for(day in 1:length(metrics.days.ls)) {
            names(metrics.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(metrics.days.ls[[day]]))
          }
        } else {
          tdr.i <- tdr.i + 1
          break
        }
        
        imagename <- paste0("dissertation_figures/spe-t2/spe-t2-",substr(dates, 1,10),
                            "-",per.var,"-",alpha*100,"-",days,"-",subsys,".png")
        png(file = imagename, width=6.5, height=5, units="in", res=900)
        par(mfcol=c(4,1), mar=c(2,2,2,1), oma=c(0,2,.5,1),family="serif")
        
        metrics.days <- metrics.days.ls[[days]]
        label.i <- 1
        label <- c("SS SPE", "MS SPE", expression("SS T"^"2"), expression("MS T"^"2"))
        for(metric in c("SPE", "T2")){ 
          for(state in c("SS","MS")) {
            if(subsys=="MT") n <- which(names(metrics.days)==paste0(state,"ADPCA-MT-",metric))
            if(subsys=="BR") n <- which(names(metrics.days)==paste0(state,"ADPCA-BR-",metric))
            # tdr.all[tdr.i,label.i] <- 
            plot.test.per.thresh.v2(metric.ls = metrics.days, 
                                    metric.n=n, y.lim = 20, type="l", 
                                    date.range=dates, n=0.75, 
                                    date.format = "%H:%M",
                                    true.alarm=NULL, minor.ticks = FALSE)
            rug(x = as.POSIXct(paste0(unique(substr(index(metrics.days[[n]]),1,13)),
                                      "00:00")), ticksize = -0.05, side = 1)
            mtext(label[label.i], side=3, line=0.5, cex=1/1.2)
            label.i <- label.i+1
          }
        }
        
        mtext("Percent of threshold", side=2, line=0.75, outer=TRUE, cex=1/1.2)
        mtext(paste0(subsys,"-",per.var,"%-",alpha*100,"%"), side=3, line=-.5, 
              outer=TRUE, cex=1/1.2)
        
        dev.off()
        tdr.i <- tdr.i + 1
      }
    }
  }
}

tdr.all.labs <- cbind(c(rep(99,14*2), rep(90,14*2), rep(80,14*2)),
                      c(rep(c(rep(1,14), rep(10,14)), 3)),
                      c(rep(1:14, 3*2)))
write.csv(cbind(tdr.all.labs, round(tdr.all,3)), file="tdr-2017-03-14.csv")

##### TDR ####
{
imagename <- paste0("dissertation_figures/tdr/tdr-",substr(dates, 1,10),
                    "-",subsys,".png")
png(file = imagename, width=6.5, height=3.5, units="in", res=900)
col.v <- c("red", "darkorange", "blue", "darkgray")
pch.v <- c(20,20,17,17)
par(family="serif", xpd=TRUE, mar=c(5,3,1,.5))
plot(x=1:nrow(tdr.all), y=rep(-1, nrow(tdr.all)),ylim=c(0,1), 
     xlim=c(1,nrow(tdr.all)+.5), xlab="", ylab="", axes=FALSE)
lines(x=c(0,nrow(tdr.all)+.5),y=c(0,0))
axis(2,pos=0)
lines(x=c(nrow(tdr.all)+.5,nrow(tdr.all)+.5),y=c(0,1))
lines(x=c(0,nrow(tdr.all)+.5),y=c(1,1))
rug(x = seq(1,nrow(tdr.all)),ticksize = -0.03, side = 1, pos=0)
for(i in 1:ncol(tdr.all)) {
  points(x=1:nrow(tdr.all), y=tdr.all[,i], pch=pch.v[i], col=alpha(col.v[i],.5))
}
for(i in seq(14.5,(14*3*2),14)) {
  lines(x=c(i,i),y=c(0,1))
}
num <- seq(14.5/2,(14*3*2),14)
num.lab <- c("99% - 1%",
             "99% - 10%",
             "90% - 1%",
             "90% - 10%",
             "80% - 1%", 
             "80% - 10%")
for(i in 1:length(num)) {
  text(x=num[i],y=1.04,labels=num.lab[i], adj=0.5)
}
for(i in as.vector(sapply(c(0:5)*14, function(x) c(3,7,11)+x))) {
  num <- i %% 14
  if(num==0) num <- 14
  text(x=i, y= -.09, adj=0.5, labels=num)
  rug(x = i,ticksize = -0.03, side = 1, pos=0, lwd=2)
}
mtext(text="Training Window (Days)", side=1, line=1.25)
mtext(text="True detection rate", side=2, line=2)
legend("bottom", legend=c("SS SPE", "MS SPE", expression("SS T"^"2"), expression("MS T"^"2")),
       pch=pch.v, col=col.v, horiz=TRUE, inset=c(0,-.4), xpd=TRUE)

dev.off()
}







##### Percent threshold #####
filename <- paste0("results/metrics-days-ls-",metrics.days.date," ",
                   per.var,"percent ",alpha*100,"alpha.RData")
load(filename)
for(day in 1:length(metrics.days.ls)) {
  names(metrics.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(metrics.days.ls[[day]]))
}
per.var <-99
alpha <- 0.01
state <- "MS"
imagename <- paste0("dissertation_figures/spe-t2/spe-t2-",substr(dates, 1,10),
                    "-",per.var,"-",alpha*100,"-",subsys,"-",state,".png")
png(file = imagename, width=6.5, height=3.5, units="in", res=900)
par(mfrow=c(2,2), mar=c(2,2,2,1), oma=c(0,2,.5,1),family="serif")

for(days in c(1,7)) {
  metrics.days <- metrics.days.ls[[days]]
  label.i <- 1
  label <- c(paste0(state," SPE ",days,"-days"), eval(bquote(expression(.(state)~T^2~.(days)*"-days"))))
  for(metric in c("SPE", "T2")){ 

      if(subsys=="MT") n <- which(names(metrics.days)==paste0(state,"ADPCA-MT-",metric))
      if(subsys=="BR") n <- which(names(metrics.days)==paste0(state,"ADPCA-BR-",metric))
      plot.test.per.thresh.v2(metric.ls = metrics.days, 
                              metric.n=n, y.lim = 20, type="l", 
                              date.range=dates, date.format = "%m-%d %H:%M", n=0.75,
                              true.alarm=NULL)
      mtext(label[label.i], side=3, line=0.5, cex=1/1.2)
      label.i <- label.i+1

  }
  

  
  
}
mtext("Percent of threshold", side=2, line=0.75, outer=TRUE, cex=1/1.2)
mtext(paste0(subsys,"-",per.var,"%-",alpha*100,"%"), side=3, line=-.5, 
      outer=TRUE, cex=1/1.2)

dev.off()