setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE, pattern=".R"), source)



##### Plot the training data #####
library(xts)
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]

## Spring 2017
load("results/results-days-ls-2017-02-26 2017-04-03.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(file="dissertation_figures/Spring 2017 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                   col.n=col.n)
dev.off()

## Summer 2017
load("results/results-days-ls-2017-05-28 2017-07-20.RData")
png(file="dissertation_figures/Summer 2017 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
plot.train.process(allData=plotdata, 
                  col.n=col.n)
dev.off()


## Winter 2017
load("results/results-days-ls-2017-09-14 2018-02-28 99percent.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))

png(file="dissertation_figures/Winter 2017 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                   col.n=col.n)
dev.off()




##### Plot all testing process variables #####
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_1\\CURRENT_MODE"]==0))),]


## Spring 2017
load("results/results-days-ls-2017-02-26 2017-04-03 90percent.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}

# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Spring 2017 90 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
png(filename="dissertation_figures/Spring 2017 90 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Spring 2017 90 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Spring 2017 90 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()





## Summer 2017
load("results/results-days-ls-2017-05-28 2017-07-20 90percent.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Summer 2017 90 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Summer 2017 90 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Summer 2017 90 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Summer 2017 90 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()




## Winter 2017
load("results/results-days-ls-2017-09-14 2018-02-28 90percent.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Winter 2017 90 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
png(filename="dissertation_figures/Winter 2017 90 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Winter 2017 90 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Winter 2017 90 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()



## Spring 2017
load("results/results-days-ls-2017-02-26 2017-04-03 99percent.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}

# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Spring 2017 99 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
png(filename="dissertation_figures/Spring 2017 99 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Spring 2017 99 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Spring 2017 99 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()





## Summer 2017
load("results/results-days-ls-2017-05-28 2017-07-20 99percent.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Summer 2017 99 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Summer 2017 99 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Summer 2017 99 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Summer 2017 99 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()




## Winter 2017
load("results/results-days-ls-2017-09-14 2018-02-28 99percent.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
# MS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(filename="dissertation_figures/Winter 2017 99 MS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
# col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()

# SS BR
# col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
png(filename="dissertation_figures/Winter 2017 99 SS ADPCA BR.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
dev.off()



# MS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Winter 2017 99 MS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
dev.off()

# SS MT
col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData[,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))

png(filename="dissertation_figures/Winter 2017 99 SS ADPCA MT.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
dev.off()




library(xts)
setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE, pattern=".R"), source)
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
for(i in 1:ncol(rawData)) {
  print(plot(rawData["2017-09-15 09:00:00/2017-10-15 13:00:00"][,i], main=colnames(rawData)[i]))
}

plot(rawData["2017-07-16 20:00:00/2017-03-24 24:00:00"][,c("MBR\\CURRENT_FLUX_MODE", 
                                                           "MBR_1\\TRANS_PRESS\\PROCESS_VALUE", 
                                                           "MBR_2\\TRANS_PRESS\\PROCESS_VALUE")])
load("results/results-days-ls-2017-09-14 2018-02-28 99percent.RData")
allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
for(day in 1:length(results.days.ls)) {
  names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
}
col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- allData["2017-10-03 00:00:00/2017-10-07 00:00:00"][,col.n]
# colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
png(filename="dissertation_figures/2017-10-05 MS ADPCA BR.png", width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "BR", state = "MS")
dev.off()
# par(mfcol=c(5,1))
plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), 
                         results.days.ls=results.days.ls, 
                         subsys = "MT", state = "MS", mfcol.c = c(5,1))




# Spring 2017
png(filename="dissertation_figures/2017-03-24 maintainence activities.png", width=6.5, height =1.5*4, units="in", res=600)
par(mfcol=c(4,1))
plotdata <- allData["2017-03-23 20:00:00/2017-03-25 01:00:00"]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
cols <- c("BIO_1\\LEVEL\\PROCESS_VALUE","BIO_1\\CURRENT_PHASE","MBR_2\\LEVEL\\PROCESS_VALUE", "MBR_2\\CURRENT_MODE")
plot.test.process.alarms(allData=plotdata, col.n=which(colnames(allData) %in% cols)[2], results.days.ls=results.days.ls, subsys = "BR", state = "SS", multiple=FALSE, legend.l = FALSE)
mtext(side=3, text="99% SS BR", font=2)
plot.test.process.alarms(allData=plotdata, col.n=which(colnames(allData) %in% cols)[4], results.days.ls=results.days.ls, subsys = "BR", state = "MS", multiple=FALSE, legend.l = FALSE)
mtext(side=3, text="99% MS BR", font=2)
plot.test.process.alarms(allData=plotdata, col.n=which(colnames(allData) %in% cols)[1], results.days.ls=results.days.ls, subsys = "MT", state = "SS", multiple=FALSE, legend.l = FALSE)
mtext(side=3, text="99% SS MT", font=2)
plot.test.process.alarms(allData=plotdata, col.n=which(colnames(allData) %in% cols)[3], results.days.ls=results.days.ls, subsys = "MT", state = "MS", multiple=FALSE)
mtext(side=3, text="99% MS MT", font=2)
dev.off()


# Summer 2017
png(filename="dissertation_figures/2017-06-16 manual to auto sewage flow.png", width=6.5, height =1.5, units="in", res=600)
par(mfcol=c(1,1))
plotdata <- allData["2017-06-15 12:00:00/2017-06-17 12:00:00"]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
cols <- c("SEWAGE\\FLOW\\PROCESS_VALUE")
plot.test.process.alarms(allData=plotdata, col.n=which(colnames(allData) %in% cols), results.days.ls=results.days.ls, subsys = "MT", state = "MS", multiple=FALSE)
mtext(side=3, text="99% MS MT", font=2)
dev.off()


### Read in metric files
load("results/metrics-days-ls-2017-02-26 2017-04-03 90percent.RData")
dates <- "2017-03-19/2017-03-25"
png(filename="dissertation_figures/2017-03-20 TMP increase 90per.png", width=6.5, height =1.5*2.2, units="in", res=600)
par(mfcol=c(2,4), mar=c(2.5,4,1,1), oma=c(0,0,3,0))

for(days in c(1,length(metrics.days.ls))) {
  metrics.days <- metrics.days.ls[[days]]
  
  n <- which(names(metrics.days)=="SSPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, date.range=dates,
                       metric.n=n, y.lim = 20)
  
  mtext(side=3, text=paste(days, "Training Days"), outer=FALSE, cex=0.8, line=0.5)
  
  n <- which(names(metrics.days)=="SSPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, date.range=dates,
                       metric.n=n, y.lim = 20)
  
  # n <- which(names(metrics.days)=="SSPCA-MT-SPE-T2")
  # plot.test.per.thresh(metric.ls = metrics.days, date.range=dates,
  #                      metric.n=n)
}
mtext(side=3, text="90% Variance, SSAD-PCA MT", outer=TRUE, cex=0.8, 
      adj=0.2, line=1.5)

for(days in c(1,length(metrics.days.ls))) {
  metrics.days <- metrics.days.ls[[days]]
  
  n <- which(names(metrics.days)=="MSADPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, date.range=dates,
                       metric.n=n, y.lim = 20)
  
  mtext(side=3, text=paste(days, "Training Days"), outer=FALSE, cex=0.8, line=0.5)
  
  
  n <- which(names(metrics.days)=="MSADPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, date.range=dates,
                       metric.n=n, y.lim = 20)
  
  # n <- which(names(metrics.days)=="MSADPCA-MT-SPE-T2")
  # plot.test.per.thresh(metric.ls = metrics.days, date.range=dates,
  #                      metric.n=n)
}
mtext(side=3, text="90% Variance, MSAD-PCA MT", outer=TRUE, cex=0.8, 
      adj=0.90, line=1.5)
dev.off()

