setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE, pattern=".R"), source)

##### Plot of ALL Alarms #####
library(xts)
sys.ls <- list() # List of SBR and MBR results
all.files <- list.files(path="results", pattern = c("results"), full.names = TRUE)
all.files <- all.files[-grep("2017-02-25 2017-04-03",all.files)]
for(subsys in c("BR", "MT")) {
  config.ls <- list () # List of each PCA config: 99% 1%, 99% 10%, etc.
  for(per.var in c(99,90,80)) {
    for(alpha in c(0.01, 0.1)) {
      files <- all.files[intersect(grep(paste0(alpha*100,"alpha"), all.files),
                                   grep(per.var, all.files))]
      # Remove fix files for now
      if(length(grep("fix", files))>0) files <- files[-grep("fix", files)] 
      files.ls <- list()
      if(length(files)>0) {
        for(f in 1:length(files)) {
          load(files[f])
          metric.state.ls <- list()
          for(day in 1:length(results.days.ls)) {
            load(files[f])
            data.ls <- results.days.ls[[day]] # 1 = One day
            names(data.ls) <- gsub("SSPCA","SSADPCA", names(data.ls))
            results.ls <- list()
            for(metric in c("SPE", "T2", "SPE-T2")) {
              
              if((metric=="SPE-T2") && (paste0(substr(files[f], 1, 24),"fix-",substr(files[f],25,nchar(files[f]))) %in% all.files)) {
                load(paste0(substr(files[f], 1, 24),"fix-",substr(files[f],25,nchar(files[f]))))
                data.ls <- results.days.ls[[day]] # 1 = One day
                names(data.ls) <- gsub("SSPCA","SSADPCA", names(data.ls))
              } else {
                
              }
              
              for(state in c("SS", "MS")) {
                n <- which(names(data.ls) == paste0(state,"ADPCA-",subsys,"-",metric))
                flag <- index(data.ls[[n]])[which(data.ls[[n]]=="OC")]
                alarms.diff <- vector()
                for(i in 3:length(flag)) {
                  alarms.diff <- c(alarms.diff, as.numeric(difftime(flag[i], flag[i-2], units = "min")))
                }
                alarms <- flag[which(alarms.diff==2)+2]
                alarms.xts <- xts(x=rep(0, nrow(data.ls[[n]])), order.by=index(data.ls[[n]]))
                alarms.xts[flag] <- 1
                alarms.xts[alarms] <- 2
                results.ls[[length(results.ls)+1]] <- alarms.xts
              }
            }
            metric.state.ls[[length(metric.state.ls)+1]] <- results.ls
          }
          plotdata.ls <- list()
          for(i in 1:length(metric.state.ls[[1]])) { ## SS SPE, MS SPE, SS T2, MS T2
            plotdata <- do.call("cbind",lapply(1:length(metric.state.ls), function(j) metric.state.ls[[j]][[i]]))
            plotdata <- plotdata[(nrow(metric.state.ls[[1]][[i]])-nrow(metric.state.ls[[length(metric.state.ls)]][[i]])+1):nrow(metric.state.ls[[1]][[i]])]
            colnames(plotdata) <- 1:ncol(plotdata)
            plotdata.ls[[length(plotdata.ls)+1]] <- plotdata
          }
          files.ls[[length(files.ls)+1]] <- plotdata.ls
        } # next file
        files.ls <- lapply(1:length(files.ls), function(n) do.call("rbind", 
                                                    lapply(files.ls, 
                                                           function(x) x[[n]])))
      }
      
      config.ls[[length(config.ls)+1]] <- files.ls
    }
  }
  sys.ls[[length(sys.ls)+1]] <- config.ls
}


add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}


for(daterange in c("2017-03-10/2017-03-17",
                   "2017-03-19/2017-03-22",
                   "2017-03-24/2017-03-31",
                   "2017-11-28/2017-12-01",
                   "2018-08-06/2018-08-08 12:00:00",
                   "2018-08-14/2018-08-15",
                   "2018-08-01/2018-09-06")) {
  for(s in 1:2) {
    png(file=paste0("dissertation_figures/testing-",s,"-",substr(daterange,1,10),".png"), width=6.5, height=8, units="in", res=600)
    par(mfrow=c(6,4), mar=c(2,3,0.5,0.5), 
        oma=c(0,1.5,2.6,2.5), cex=1, cex.main=1, xpd=FALSE)
    for(r in 1:length(sys.ls[[s]])) {
      per.var.key <- c(99,99,90,90,80,80)
      alpha.key <- c(0.01, 0.1, 0.01, 0.1,0.01, 0.1)
      if(length(sys.ls[[s]][[r]])==0) {
        plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
        plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
        plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
        plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
        next
      }
      for(c in 1:length(sys.ls[[s]][[r]])) {
        data.master <- sys.ls[[s]][[r]][[1]][daterange]
        data.all <- sys.ls[[s]][[r]][[c]][daterange]
        if(length(data.all)==0) {
          plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
          next
        }
        plot(x=index(data.master),y=rep(0,nrow(data.master)),ylim=c(1,14), xlab="", ylab="")
        for(i in 1:14){
          data <- data.all[,i]
          flags <- which(data==1)
          alarms <- which(data==2)
          if(length(flags)>0) points(x=index(data[flags,]), y=rep(i,length(flags)), 
                                     col="grey", cex=.25)
          if(length(alarms)>0) points(x=index(data[alarms,]), y=rep(i,length(alarms)), 
                                      col="black", cex=.25)
          
        }
        if(r==1) {
          if(c==1) mtext("SS SPE", side=3, line=.25, font=2)
          if(c==2) mtext("MS SPE", side=3, line=.25, font=2)
          if(c==3) mtext("SS T2", side=3, line=.25, font=2)
          if(c==4) mtext("MS T2", side=3, line=.25, font=2)
        }
        if(c==4) {
          corners <- par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
          # par(xpd = TRUE) #Draw outside plot area
          text(x = corners[2]+(corners[2]-corners[1])/5, y = mean(corners[3:4]), 
               paste0(alpha.key[r]*100,"% sig"), srt = 270, xpd=NA, font=2)
          text(x = corners[2]+(corners[2]-corners[1])/5*2.5, y = mean(corners[3:4]), 
               paste0(per.var.key[r],"% var"), srt = 270, xpd=NA, font=2)
          # par(xpd=FALSE)
          # mtext(paste0(per.var.key[r],"% var"), side=4, line=2.75)
          # mtext(paste0(alpha.key[r]*100,"% sig"), side=4, line=1.5)
        }
      }
    }
    mtext("Training Window Size (days)", side=2, outer=TRUE, font=2)
    if(s==1) mtext("SBR", side=3, outer=TRUE, line=1.1, font=2)
    if(s==2) mtext("MBR", side=3, outer=TRUE, line=1.1, font=2)
    
    add_legend("topleft", legend=c("Flag", "Alarm"), pch=1, 
               col=c("grey", "black"),
               horiz=FALSE, bty="n",cex=1)
    dev.off()
  }
  
}

