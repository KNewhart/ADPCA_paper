setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE), source)



##### Plot the training data #####
library(xts)
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]

# Spring 2017
load("results/results-days-ls-2017-02-26 2017-04-03.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(file="dissertation_figures/Spring 2017 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                   col.n=col.n)
dev.off()

# Summer 2017
load("results/results-days-ls-2017-05-28 2017-07-20.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE)))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(file="dissertation_figures/Summer 2017 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                  col.n=col.n)
dev.off()





###### Plot all testing process variables #####
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_1\\CURRENT_MODE"]==0))),]


# Spring 2017
load("results/results-days-ls-2017-02-26 2017-04-03.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(data.ls))
}

# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Spring 2017 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
png(filename="dissertation_figures/Spring 2017 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Spring 2017 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Spring 2017 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()





# Summer 2017
load("results/results-days-ls-2017-05-28 2017-07-20.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(data.ls))
}
# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Summer 2017 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
png(filename="dissertation_figures/Summer 2017 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Summer 2017 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Summer 2017 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()





