setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\Fault detection\\R\\clean")
sapply(list.files(path="src", full.names=TRUE, pattern=".R"), source)



##### Plot the training data #####
library(xts)
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]

load("results/results-days-ls-2017-02-26 2017-04-03 99percent 1alpha.RData")
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
colnames(plotdata) <- seq(1,ncol(plotdata))
library(corrplot)
col2 <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061")))
foo <- corrplot(cor(scale(plotdata)), 
                type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, 
                col = col2(200))
plotdata <- plotdata[,as.numeric(dimnames(foo)[[1]])]
colnames(plotdata) <- seq(1,ncol(plotdata))

png("dissertation_figures/cor-plot.png", width=6.5, height = 6.5, units="in", res=600)
par(cex=0.8)
# layout(mat=matrix(c(1,2),nrow=1), widths = c(0.3,0.7))
# plot(-10,axes=FALSE, xlim=c(0,10), ylim=c(0,10), xlab="", ylab="")
# 
# label.mat <- matrix(c(rep(0.25,length(seq(10,-1.5,by=-0.5))),
#                       rep(10,ncol(plotdata)-length(seq(10,-1.5,by=-0.5))),
#                       seq(10,-1.5,by=-0.5), 
#                     c(-.5+seq(0,ncol(plotdata)-length(seq(10,-1.5,by=-0.5)))*.5)),
#                     byrow = FALSE, ncol=2)
# text(x=label.mat[,1],
#      y=label.mat[,2],
#      1:ncol(plotdata), adj=0)
# var.names <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(rawData)[grep("PROCESS_VALUE", colnames(rawData))])))
# text(x=label.mat[,1]+1,
#      y=label.mat[,2],
#      var.names[as.numeric(dimnames(foo)[[1]])],
#      adj=0)

# par(cex=0.8, mar=c(0,0,0,0), xpd=TRUE)
corrplot(cor(scale(plotdata)), 
                type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, col = col2(200))

dev.off()

png("dissertation_figures/cor-plot-br.png", width=6.5, height = 6.5, units="in", res=600)
par(cex=0.8)
library(corrplot)
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
state.train <-plotdata[,which(colnames(plotdata) %in% c(stateVarsBR, stateVarsMT))]
state.train <- cbind("BR.State"=apply(plotdata[,stateVarsBR],1,paste,collapse=""),
                     "MT.State"=apply(plotdata[,stateVarsMT],1,paste,collapse=""))
state.train <- xts(state.train, order.by=index(plotdata))
state <- unique(state.train[,1])[which(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x)))==max(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x)))))]
plotdata <- plotdata[which(state.train[,1]==state), varsBR]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
plotdata <- plotdata[,-2]
colnames(plotdata) <- seq(1,ncol(plotdata))
foo <- corrplot(cor(scale(plotdata)), 
                type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, col = col2(200))
plotdata <- plotdata[,as.numeric(dimnames(foo)[[1]])]
colnames(plotdata) <- seq(1,ncol(plotdata))
corrplot(cor(scale(plotdata)), 
         type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, col = col2(200))
dev.off()

plotdata <- rawData[,varsBR]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
plotdata <- plotdata[,-2]
colnames(plotdata)[as.numeric(dimnames(foo)[[1]])]


##### Covariance matrix

library(eqs2lavaan)
# par(mfrow=c(1,3))
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]
load("results/results-days-ls-2017-02-26 2017-04-03 99percent.RData")
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
plotdata <- plotdata[,c(varsBR[-which(varsBR %in% varsMT)], 
                        varsBR[which(varsBR %in% varsMT)], 
                        varsMT[-which(varsMT %in% varsBR)])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
var.matrix <- colnames(plotdata)
colnames(plotdata) <- seq(1,ncol(plotdata))
cov.plotdata <- cov(scale(plotdata))
# plotCov(cov.plotdata)

V <- cov.plotdata
R <- cov2cor(V)
V[lower.tri(V)] <- 0
R[upper.tri(R, diag = TRUE)] <- 0

pdf(file="dissertation_figures/cov-plot.pdf", width=6.5, height = 6.5)
par(mar = c(5, 5, 2, 2), xpd=FALSE, family="serif")
# image(abs(V + R), 
#       # main = "Correlation (Upper); Covariance (Lower)", 
#       xaxt = "n", yaxt = "n")
fields::image.plot(abs(V + R), 
                   main = "SB-MBR SS", 
                   xaxt = "n", yaxt = "n", col = hcl.colors(12, "YlOrRd", rev = TRUE))
axis(2, at = seq(0, 1, 1/((dim(V)[1]) - 1)),
     labels = rownames(V), 
     las = 2, cex.axis = 1)
axis(1, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = rownames(V), 
     las = 1, cex.axis = 1)
par(new=TRUE)
plot(x=-10, y=-10, xlim=c(2-.2,34+.2), ylim=c(2-.2,34+.2), axes=FALSE,
     xlab="", ylab="")
# lines(x=c(tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5,
#           tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5),
#        y=c(0,tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5))
# lines(y=c(tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5,
#           tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5),
#       x=c(0,tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5))
# lines(x=c(head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+.5,
#           head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+.5),
#       y=c(0,head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+.5))
# lines(y=c(head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+.5,
#           head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+.5),
#       x=c(0,head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+.5))
abline(v=tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5, lty=2)
abline(v=head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)-.5, lty=2)
abline(h=tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5, lty=2)
abline(h=head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)-.5, lty=2)
par(xpd=TRUE)
text("SBR", 
     x=(tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5)/2,
     y=-5)
text("SBR", 
     y=(tail(which(var.matrix %in% varsBR[-which(varsBR %in% varsMT)]),n=1)+.5)/2,
     x=-5)
text("MBR", 
     x=head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+
       (length(var.matrix) - head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1))/2+.5,
     y=-5)
text("MBR", 
     y=head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1)+
       (length(var.matrix) - head(which(var.matrix %in% varsMT[-which(varsMT %in% varsBR)]),n=1))/2+.5,
     x=-5)
dev.off()

##### BR Covariance #####
pdf(file="dissertation_figures/cov-plot-br.pdf", width=6.5*2, height = 6.5)
par(mfrow=c(1,2))
par(mar = c(3, 3, 2, .5))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
state.train <-plotdata[,which(colnames(plotdata) %in% c(stateVarsBR, stateVarsMT))]
state.train <- cbind("BR.State"=apply(plotdata[,stateVarsBR],1,paste,collapse=""),
                     "MT.State"=apply(plotdata[,stateVarsMT],1,paste,collapse=""))
state.train <- xts(state.train, order.by=index(plotdata))

state <- unique(state.train[,1])[which(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x)))==max(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x)))))]
plotdata <- plotdata[which(state.train[,1]==state), ]
plotdata <- plotdata[,c(varsBR[-which(varsBR %in% varsMT)], 
                        varsBR[which(varsBR %in% varsMT)])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
var.matrix.br <- colnames(plotdata)
colnames(plotdata) <- seq(1,ncol(plotdata))
cov.plotdata <- cov(scale(plotdata))
# plotCov(cov.plotdata)
V <- cov.plotdata
R <- cov2cor(V)
V[lower.tri(V)] <- 0
R[upper.tri(R, diag = TRUE)] <- 0
image(V + R, 
      main = "SBR State #1", 
      xaxt = "n", yaxt = "n", col = hcl.colors(12, "YlOrRd", rev = TRUE))
axis(2, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = rownames(V), 
     las = 2, cex.axis = 1)
axis(1, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = rownames(V), 
     las = 2, cex.axis = 1)

plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
state <- unique(state.train[,1])[which(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x)))==head(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x))), n=2)[2])]
plotdata <- plotdata[which(state.train[,1]==state), ]
plotdata <- plotdata[,c(varsBR[-which(varsBR %in% varsMT)], 
                        varsBR[which(varsBR %in% varsMT)])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
var.matrix.br <- colnames(plotdata)
colnames(plotdata) <- seq(1,ncol(plotdata))
cov.plotdata <- cov(scale(plotdata))
# plotCov(cov.plotdata)
V <- cov.plotdata
R <- cov2cor(V)
V[lower.tri(V)] <- 0
R[upper.tri(R, diag = TRUE)] <- 0
fields::image.plot(V + R, 
      main = "SBR State #2", 
      xaxt = "n", yaxt = "n", col = hcl.colors(12, "YlOrRd", rev = TRUE))
axis(2, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = rownames(V), 
     las = 2, cex.axis = 1)
axis(1, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = rownames(V), 
     las = 2, cex.axis = 1)
dev.off()



## Spring 2017
load("results/results-days-ls-2017-02-26 2017-04-03 99 percent.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(file="dissertation_figures/Spring 2017 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                   col.n=col.n)
dev.off()

## Summer 2017
load("results/results-days-ls-2017-05-28 2017-07-20 99 percent.RData")
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


## Summer 2018
load("results/results-days-ls-2018-05-01 2018-09-05 99percent.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))

png(file="dissertation_figures/Summer 2018 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                   col.n=col.n)
dev.off()



##### BR Covariance Difference #####
pdf(file="dissertation_figures/cov-plot-br-diff.pdf", width=6.5, height = 6.5)
par(mfrow=c(1,1), family="serif")
par(mar = c(3, 3, 2, .5))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
state.train <-plotdata[,which(colnames(plotdata) %in% c(stateVarsBR, stateVarsMT))]
state.train <- cbind("BR.State"=apply(plotdata[,stateVarsBR],1,paste,collapse=""),
                     "MT.State"=apply(plotdata[,stateVarsMT],1,paste,collapse=""))
state.train <- xts(state.train, order.by=index(plotdata))

state <- names(sort(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x))), decreasing = TRUE))[1]
plotdata <- plotdata[which(state.train[,1]==state), ]
plotdata <- plotdata[,c(varsBR[-which(varsBR %in% varsMT)], 
                        varsBR[which(varsBR %in% varsMT)])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
var.matrix.br <- colnames(plotdata)
colnames(plotdata) <- seq(1,ncol(plotdata))
cov.plotdata <- cov(scale(plotdata))
# plotCov(cov.plotdata)
V <- cov.plotdata
R <- cov2cor(V)
V[lower.tri(V)] <- 0
R[upper.tri(R, diag = TRUE)] <- 0
state.1 <- V+R
diag(state.1) <- 1

plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
state <- names(sort(sapply(unique(state.train[,1]), function(x) length(which(state.train[,1]==x))), decreasing = TRUE))[2]
plotdata <- plotdata[which(state.train[,1]==state), ]
plotdata <- plotdata[,c(varsBR[-which(varsBR %in% varsMT)], 
                        varsBR[which(varsBR %in% varsMT)])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
var.matrix.br <- colnames(plotdata)
colnames(plotdata) <- seq(1,ncol(plotdata))
cov.plotdata <- cov(scale(plotdata))
# plotCov(cov.plotdata)
V <- cov.plotdata
R <- cov2cor(V)
V[lower.tri(V)] <- 0
R[upper.tri(R, diag = TRUE)] <- 0
state.2 <- V+R

state.diff <- state.1
for(i in 1:length(state.1)) {
  if(is.na(state.1[i])) state.1[i] <- 0
  state.diff[i] <- abs(state.1[i]-state.2[i])
}


fields::image.plot(state.diff, 
                   main = "SBR State #1 - #2", 
                   xaxt = "n", yaxt = "n", col = hcl.colors(12, "YlOrRd", rev = TRUE),
                   zlim=c(0,1))
axis(2, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = rownames(V), 
     las = 2, cex.axis = 1)
axis(1, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = rownames(V), 
     las = 1, cex.axis = 1)
dev.off()


##### MT Covariance Difference #####
pdf(file="dissertation_figures/cov-plot-mt-diff.pdf", width=6.5, height = 6.5)
par(mfrow=c(1,1), family="serif")
par(mar = c(3, 3, 2, .5))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
state.train <-plotdata[,which(colnames(plotdata) %in% c(stateVarsBR, stateVarsMT))]
state.train <- cbind("BR.State"=apply(plotdata[,stateVarsBR],1,paste,collapse=""),
                     "MT.State"=apply(plotdata[,stateVarsMT],1,paste,collapse=""))
state.train <- xts(state.train, order.by=index(plotdata))

state <- names(sort(sapply(unique(state.train[,2]), function(x) length(which(state.train[,2]==x))), decreasing = TRUE))[1]
plotdata <- plotdata[which(state.train[,2]==state), ]
plotdata <- plotdata[,c(varsMT[-which(varsMT %in% varsBR)], 
                        varsMT[which(varsMT %in% varsBR)])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
var.matrix.br <- colnames(plotdata)
colnames(plotdata) <- seq(1,ncol(plotdata))
cov.plotdata <- cov(scale(plotdata))
# plotCov(cov.plotdata)
V <- cov.plotdata
R <- cov2cor(V)
V[lower.tri(V)] <- 0
R[upper.tri(R, diag = TRUE)] <- 0
state.1 <- V+R
diag(state.1) <- 1

plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
state <- names(sort(sapply(unique(state.train[,2]), function(x) length(which(state.train[,2]==x))), decreasing = TRUE))[2]
plotdata <- plotdata[which(state.train[,2]==state), ]
plotdata <- plotdata[,c(varsMT[-which(varsMT %in% varsBR)], 
                        varsMT[which(varsMT %in% varsBR)])]
plotdata <- plotdata[,grep("PROCESS_VALUE",colnames(plotdata))]
var.matrix.br <- colnames(plotdata)
colnames(plotdata) <- seq(1,ncol(plotdata))
cov.plotdata <- cov(scale(plotdata))
# plotCov(cov.plotdata)
V <- cov.plotdata
R <- cov2cor(V)
V[lower.tri(V)] <- 0
R[upper.tri(R, diag = TRUE)] <- 0
state.2 <- V+R
diag(state.1) <- 1

state.diff <- state.1
for(i in 1:length(state.1)) {
  if(is.na(state.1[i])) state.1[i] <- 0
  if(is.na(state.2[i])) state.2[i] <- 0
  state.diff[i] <- abs(state.1[i]-state.2[i])
}


fields::image.plot(state.diff, 
                   main = "MBR State #1 - #2", 
                   xaxt = "n", yaxt = "n", col = hcl.colors(12, "YlOrRd", rev = TRUE),
                   zlim=c(0,1))
axis(2, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = 12:35, 
     las = 2, cex.axis = 1)
axis(1, at = seq(0, 1, 1/((dim(V)[1]) - 1)), labels = 12:35, 
     las = 1, cex.axis = 1)
dev.off()






## Spring 2017
load("results/results-days-ls-2017-02-26 2017-04-03 99 percent.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
png(file="dissertation_figures/Spring 2017 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                   col.n=col.n)
dev.off()

## Summer 2017
load("results/results-days-ls-2017-05-28 2017-07-20 99 percent.RData")
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


## Summer 2018
load("results/results-days-ls-2018-05-01 2018-09-05 99percent.RData")
col.n <- which(colnames(rawData) %in% unlist(sapply(unique(c(varsBR, varsMT)), function(x) grep("PROCESS_VALUE", x, value=TRUE))))
plotdata <- rawData[paste0(index(results.days.ls[[1]][[1]])[1]-24*60*60,"/",index(results.days.ls[[length(results.days.ls)]][[1]])[1])]
colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))

png(file="dissertation_figures/Summer 2018 Timeseries.png", width=6.5, height =1.5*length(col.n), units="in", res=600)
plot.train.process(allData=plotdata, 
                   col.n=col.n)
dev.off()






##### Plot all testing process variables #####
load("results/rawData.RData")
rawData <- rawData[-unique(c(which(rawData[,"MBR_1\\CURRENT_MODE"]==0),
                             which(rawData[,"MBR_2\\CURRENT_MODE"]==0))),]

## Spring 2017
alpha <- 0.01
for(per.var in c(99,90,80)) {
  filename <- paste0("results/results-days-ls-2017-02-26 2017-04-03 ",per.var,"percent ",alpha*100,"alpha.RData")
  if(!(filename %in% list.files("results", full.names = TRUE))) next
  load(filename)
  allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
  for(day in 1:length(results.days.ls)) {
    names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
  }
  
  # MS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
  png(filename=paste0("dissertation_figures/Spring 2017 ",per.var," ", alpha*100," MS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
  dev.off()
  
  setwd("dissertation_figures/zoom-in")
  subsys <- "BR"
  for(state in c("SS", "MS")) {
    zoom.in(results.days.ls, plotdata, col.n=col.n, subsys, state, per.var, alpha)
  }
  setwd("../..")
  
  # SS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  png(filename=paste0("dissertation_figures/Spring 2017 ",per.var," ", alpha*100," SS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
  dev.off()
  
  # MS MT
  col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  
  png(filename=paste0("dissertation_figures/Spring 2017 ",per.var," ", alpha*100," MS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
  dev.off()
  
  setwd("dissertation_figures/zoom-in")
  subsys <- "MT"
  for(state in c("SS", "MS")) {
    zoom.in(results.days.ls, plotdata, col.n=1:ncol(plotdata), subsys, state, per.var, alpha)
  }
  setwd("../..")
  
  # SS MT
  col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  
  png(filename=paste0("dissertation_figures/Spring 2017 ",per.var," ", alpha*100," SS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
  dev.off()
  
}


## Summer 2017
for(per.var in c(99,90,80)) {
  filename <- paste0("results/results-days-ls-2017-05-28 2017-07-20 ",per.var,"percent ",alpha*100,"alpha.RData")
  if(!(filename %in% list.files("results", full.names = TRUE))) next
  load(filename)
  allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
  for(day in 1:length(results.days.ls)) {
    names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
  }
  
  # MS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(allData))))
  
  png(filename=paste0("dissertation_figures/Summer 2017 ",per.var," ", alpha*100," MS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
  dev.off()
  
  setwd("dissertation_figures/zoom-in")
  subsys <- "BR"
  for(state in c("SS", "MS")) {
    zoom.in(results.days.ls, plotdata, col.n=1:ncol(plotdata), subsys, state, per.var, alpha)
  }
  setwd("../..")
  
  # SS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  png(filename=paste0("dissertation_figures/Summer 2017 ",per.var," ", alpha*100," SS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
  dev.off()
  
  # MS MT
  col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  
  png(filename=paste0("dissertation_figures/Summer 2017 ",per.var," ", alpha*100," MS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
  dev.off()
  
  setwd("dissertation_figures/zoom-in")
  subsys <- "MT"
  for(state in c("SS", "MS")) {
    zoom.in(results.days.ls, plotdata, col.n=1:ncol(plotdata), subsys, state, per.var, alpha)
  }
  setwd("../..")
  
  # SS MT
  col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  
  png(filename=paste0("dissertation_figures/Summer 2017 ",per.var," ", alpha*100," SS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
  dev.off()
  
}


## Winter 2017
for(per.var in c(99,90,80)) {
  filename <- paste0("results/results-days-ls-2017-09-14 2018-02-28 ",per.var,"percent ",alpha*100,"alpha.RData")
  if(!(filename %in% list.files("results", full.names = TRUE))) next
  load(filename)
  allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
  for(day in 1:length(results.days.ls)) {
    names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
  }
  
  # MS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  png(filename=paste0("dissertation_figures/Winter 2017 ",per.var," ", alpha*100," MS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "BR", state = "MS")
  dev.off()
  setwd("dissertation_figures/zoom-in")
  subsys <- "BR"
  for(state in c("SS", "MS")) {
    zoom.in(results.days.ls, plotdata, col.n=ncol(plotdata):1, subsys, state, per.var, alpha)
  }
  setwd("../..")
  
  
  # SS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  png(filename=paste0("dissertation_figures/Winter 2017 ",per.var," ", alpha*100," SS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
  dev.off()
  
  # MS MT
  col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  
  png(filename=paste0("dissertation_figures/Winter 2017 ",per.var," ", alpha*100," MS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
  dev.off()
  
  setwd("dissertation_figures/zoom-in")
  subsys <- "MT"
  for(state in c("SS", "MS")) {
    zoom.in(results.days.ls, plotdata, col.n=1:ncol(plotdata), subsys, state, per.var, alpha)
  }
  setwd("../..")
  
  # SS MT
  col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  
  png(filename=paste0("dissertation_figures/Winter 2017 ",per.var," ", alpha*100," SS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
  dev.off()
  
}


## Summer 2018
for(alpha in c(0.01, 0.1)) {
  for(per.var in c(99,90,80)) {
    filename <- paste0("results/results-days-ls-2018-05-01 2018-09-05 ",per.var,"percent ",alpha*100,"alpha.RData")
    if(!(filename %in% list.files("results", full.names = TRUE))) next
    load(filename)
    allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
    for(day in 1:length(results.days.ls)) {
      names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
    }
    
    # MS BR
    # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
    # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    # plotdata <- allData[,col.n]
    # colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
    # png(filename=paste0("dissertation_figures/Summer 2018 ",per.var," ", alpha*100," MS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
    # # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    # plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "MS")
    # dev.off()
    
    col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    plotdata <- allData[,col.n]
    plotdata <- plotdata["2018-08-01/"]
    colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
    png(filename=paste0("dissertation_figures/August 2018 ",per.var," ", alpha*100," MS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
    plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "BR", state = "MS")
    dev.off()
    
    col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    plotdata <- allData[,col.n]
    plotdata <- plotdata["2018-08-01/"]
    colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
    png(filename=paste0("dissertation_figures/August 2018 ",per.var," ", alpha*100," SS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
    plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), 
                             results.days.ls=results.days.ls, 
                             subsys = "BR", state = "SS")
    dev.off()
    
    # setwd("dissertation_figures/zoom-in")
    # subsys <- "BR"
    # for(state in c("SS", "MS")) {
    #   zoom.in(results.days.ls, plotdata, 1:ncol(plotdata), subsys, state, per.var, alpha)
    # }
    # setwd("../..")
    
    # SS BR
    # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
    # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    # png(filename=paste0("dissertation_figures/Summer 2018 ",per.var," ", alpha*100," SS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
    # plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
    # dev.off()
    
    # MS MT
    # col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    # plotdata <- allData[,col.n]
    # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
    # colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
    # png(filename=paste0("dissertation_figures/Summer 2018 ",per.var," ", alpha*100," MS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
    # plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
    # dev.off()
    
    col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    plotdata <- allData[,col.n]
    plotdata <- plotdata["2018-08-01/"]
    colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
    png(filename=paste0("dissertation_figures/August 2018 ",per.var," ", alpha*100," MS ADPCA MT.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
    plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), 
                             results.days.ls=results.days.ls, 
                             subsys = "MT", state = "MS")
    dev.off()
    
    col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    plotdata <- allData[,col.n]
    plotdata <- plotdata["2018-08-01/"]
    colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
    png(filename=paste0("dissertation_figures/August 2018 ",per.var," ", alpha*100," SS ADPCA MT.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
    plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), 
                             results.days.ls=results.days.ls, 
                             subsys = "MT", state = "SS")
    dev.off()
    
    # 
    # setwd("dissertation_figures/zoom-in")
    # subsys <- "MT"
    # for(state in c("SS", "MS")) {
    #   zoom.in(results.days.ls, plotdata, 1:ncol(plotdata), subsys, state, per.var, alpha)
    # }
    # setwd("../..")
    
    # SS MT
    # col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    # plotdata <- allData[,col.n]
    # # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
    # colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
    # 
    # png(filename=paste0("dissertation_figures/Summer 2018 ",per.var," ", alpha*100," SS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
    # plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
    # dev.off()
  
  }
}

## Fall 2019
for(alpha in c(0.01 ,0.1)) {
for(per.var in c(99,90,80)) {
  filename <- paste0("results/results-days-ls-2019-07-01 2020-01-13 ",per.var,"percent ",alpha*100,"alpha.RData")
  if(!(filename %in% list.files("results", full.names = TRUE))) next
  load(filename)
  allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
  for(day in 1:length(results.days.ls)) {
    names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
  }
  
  # MS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  png(filename=paste0("dissertation_figures/Fall 2019 ",per.var," ", alpha*100," MS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "BR", state = "MS")
  dev.off()
  # setwd("dissertation_figures/zoom-in")
  # subsys <- "BR"
  # for(state in c("SS", "MS")) {
  #   zoom.in(results.days.ls, plotdata, 1:ncol(plotdata), subsys, state, per.var, alpha)
  # }
  # setwd("../..")
  
  # SS BR
  # col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE", "BIO_2\\LEVEL\\PROCESS_VALUE", "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
  # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  png(filename=paste0("dissertation_figures/Fall 2019 ",per.var," ", alpha*100," SS ADPCA BR.png"), width=6.5, height =1.5*length(col.n), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=col.n, results.days.ls=results.days.ls, subsys = "BR", state = "SS")
  dev.off()
  
  # MS MT
  col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  plotdata <- allData[,col.n]
  # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  
  png(filename=paste0("dissertation_figures/Fall 2019 ",per.var," ", alpha*100," MS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "MS")
  dev.off()
  # 
  # setwd("dissertation_figures/zoom-in")
  # subsys <- "MT"
  # for(state in c("SS", "MS")) {
  #   zoom.in(results.days.ls, plotdata, 1:ncol(plotdata), subsys, state, per.var, alpha)
  # }
  # setwd("../..")
  
  # SS MT
  # col.n <- which(colnames(allData) %in% unlist(sapply(varsMT, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
  # plotdata <- allData[,col.n]
  # # colnames(plotdata) <- c("Permeate Conductivity (uS/cm)", "MBR 2 Level (ft)", "MBR 1 Transmembrane Pressure (psi)", "MBR 1 Total Suspended Solids (mg/L)")
  # colnames(plotdata) <- gsub("_", " ", gsub("[\\]", " ", gsub("[\\]PROCESS_VALUE", "", colnames(plotdata))))
  # 
  png(filename=paste0("dissertation_figures/Fall 2019 ",per.var," ", alpha*100," SS ADPCA MT.png"), width=6.5, height =1.5*ncol(plotdata), units="in", res=600)
  plot.test.process.alarms(allData=plotdata, col.n=1:ncol(plotdata), results.days.ls=results.days.ls, subsys = "MT", state = "SS")
  dev.off()
  
}
}

















##### Plot all testing SPE/T2 #####
metrics.days.date <- "2018-05-01 2018-09-05"
dates <- "2018-08-01 00:00:00/"

for(subsys in c("BR", "MT")) {
  for(alpha in c(0.01, 0.1)) {
    for(days in 1:14) {
      for(per.var in c(99,90,80)) {

        filename <- paste0("results/metrics-days-ls-",metrics.days.date," ",per.var,"percent ",alpha*100,"alpha.RData")
        if(filename %in% list.files(recursive = TRUE)) {
          load(filename)
        } else {
          break
        }
        
        if(per.var==99) {
          png(file = paste0("dissertation_figures/spe-t2-",substr(dates, 1,10),"-",alpha*100,"-",days,"-",subsys,".png"), width=6.5, height=5, units="in", res=600)
          par(mfrow=c(3,4), mar=c(2.5,4,1,1), oma=c(1,5,1,0))
        }
        
        metrics.days <- metrics.days.ls[[days]]
        
        if(subsys=="MT") n <- which(names(metrics.days)=="SSPCA-MT-SPE")
        if(subsys=="BR") n <- which(names(metrics.days)=="SSPCA-BR-SPE")
        plot.test.per.thresh(metric.ls = metrics.days, 
                             metric.n=n, y.lim = 20, type="p", date.range=dates, n=.5)
        
        mtext(side=2, text=paste0(per.var, "% Variance"), outer=FALSE, cex=0.8, line=5, font=2)
        mtext(side=2, text=paste0(alpha*100, "% Threshold"), outer=FALSE, cex=0.8, line=7, font=2)
        if(per.var==99) mtext(side=3, text="Single-state SPE", line=1, cex=0.8, font=2)
        
        if(subsys=="MT") n <- which(names(metrics.days)=="MSADPCA-MT-SPE")
        if(subsys=="BR") n <- which(names(metrics.days)=="MSADPCA-BR-SPE")
        plot.test.per.thresh(metric.ls = metrics.days, 
                             metric.n=n, y.lim = 20, type="p", date.range=dates, n=.5)
        if(per.var==99) mtext(side=3, text="Multi-state SPE", line=1, cex=0.8, font=2)
        
        
        
        if(subsys=="MT") n <- which(names(metrics.days)=="SSPCA-MT-T2")
        if(subsys=="BR") n <- which(names(metrics.days)=="SSPCA-BR-T2")
        plot.test.per.thresh(metric.ls = metrics.days, 
                             metric.n=n, y.lim = 20, type="p", date.range=dates, n=.5)
        
        if(per.var==99) mtext(side=3, text="Single-state T2", line=1, cex=0.8, font=2)
        if(per.var==90) {
          mtext("Time (days)", side=1, outer=TRUE, cex=0.7, font=1)
        }
        
        if(subsys=="MT") n <- which(names(metrics.days)=="MSADPCA-MT-T2")
        if(subsys=="BR") n <- which(names(metrics.days)=="MSADPCA-BR-T2")
        plot.test.per.thresh(metric.ls = metrics.days, 
                             metric.n=n, y.lim = 20, type="p", date.range=dates, n=.5)
        if(per.var==99) mtext(side=3, text="Multi-state T2", line=1, cex=0.8, font=2)
        
      }
      dev.off()
    }
  }
}








## Summer 2017 alpha 1%
alpha <- .01
per.var <- 99
# for(days in c(1,7,14)) {
days <- 14
# pdf(file = "dissertation_figures/spe-t2-2017-03-28.pdf", width=6.5, height=5.5)
png(file = "dissertation_figures/spe-t2-2017-summer-1alpha-14d.png", width=6.5, height=4.5, units="in", res=600)
par(mfrow=c(3,4), mar=c(2.5,4,1,1), oma=c(0,3,1,0))
for(per.var in c(99,90,80)) {
  filename <- paste0("results/metrics-days-ls-2017-05-28 2017-07-20 ",per.var,"percent ",alpha*100,"alpha.RData")
  load(filename)
  dates <- paste0(range(index(metrics.days.ls[[length(metrics.days.ls)]][[1]])), collapse="/")
  # dates <- "2017-03-27/2017-03-29"
  metrics.days <- metrics.days.ls[[days]]
  
  n <- which(names(metrics.days)=="SSPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, 
                       type="p", date.range=dates,
                       n=0.5)
  
  mtext(side=2, text=paste0(per.var, "% Variance"), outer=FALSE, cex=0.8, line=5, font=2)
  if(per.var==99) mtext(side=3, text="Single-state SPE", line=1, cex=0.8, font=2)
  
  n <- which(names(metrics.days)=="MSADPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, 
                       type="p", date.range=dates,
                       n=0.5)
  if(per.var==99) mtext(side=3, text="Multi-state SPE", line=1, cex=0.8, font=2)
  
  
  
  n <- which(names(metrics.days)=="SSPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, 
                       type="p", date.range=dates,
                       n=0.5)
  
  if(per.var==99) mtext(side=3, text="Single-state T2", line=1, cex=0.8, font=2)
  
  n <- which(names(metrics.days)=="MSADPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, 
                       type="p", date.range=dates,
                       n=0.5)
  if(per.var==99) mtext(side=3, text="Multi-state T2", line=1, cex=0.8, font=2)
  
}
dev.off()





## Spring 2017 alpha 10%
pdf(file = "dissertation_figures/spe-t2-spring2017.pdf", width=6.5, height=3.25)
per.var <- 99
par(mfrow=c(2,4), mar=c(2.5,4,2.5,1), oma=c(0,3,1,0))
for(days in 1:14) {
  filename <- paste0("results/metrics-days-ls-2017-02-26 2017-04-03 ",per.var,"percent 10alpha.RData")
  load(filename)
  dates <- paste0(range(index(metrics.days.ls[[length(metrics.days.ls)]][[1]])), collapse="/")
  
  metrics.days <- metrics.days.ls[[days]]
  
  n <- which(names(metrics.days)=="SSPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  mtext(side=2, text="10% Threshold", outer=FALSE, cex=0.8, line=5, font=2)
  mtext(side=3, text="Single-state SPE", line=1.5, cex=0.8, font=2)
  
  n <- which(names(metrics.days)=="MSADPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  mtext(side=3, text="Multi-state SPE", line=1.5, cex=0.8, font=2)
  
  
  
  n <- which(names(metrics.days)=="SSPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  mtext(side=3, text="Single-state T2", line=1.5, cex=0.8, font=2)
  
  n <- which(names(metrics.days)=="MSADPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  mtext(side=3, text="Multi-state T2", line=1.5, cex=0.8, font=2)
  
  filename <- paste0("results/metrics-days-ls-2017-02-26 2017-04-03 ",per.var,"percent 1alpha.RData")
  load(filename)
  metrics.days <- metrics.days.ls[[days]]
  
  n <- which(names(metrics.days)=="SSPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  mtext(side=2, text="1% Threshold", outer=FALSE, cex=0.8, line=5, font=2)
  
  
  n <- which(names(metrics.days)=="MSADPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  
  
  
  n <- which(names(metrics.days)=="SSPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  
  
  n <- which(names(metrics.days)=="MSADPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
}

## Summer 2017 alpha 10%
library(xts)
pdf(file = "dissertation_figures/spe-t2-summer2017.pdf", width=6.5, height=3.25)
per.var <- 99
par(mfrow=c(2,4), mar=c(2.5,4,2.5,1), oma=c(0,3,1,0))
for(days in 1:14) {
  filename <- paste0("results/metrics-days-ls-2017-05-28 2017-07-20 ",per.var,"percent 10alpha.RData")
  load(filename)
  dates <- paste0(range(index(metrics.days.ls[[length(metrics.days.ls)]][[1]])), collapse="/")
  
  metrics.days <- metrics.days.ls[[days]]
  
  n <- which(names(metrics.days)=="SSPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  mtext(side=2, text="10% Threshold", outer=FALSE, cex=0.8, line=5, font=2)
  mtext(side=3, text="Single-state SPE", line=1.5, cex=0.8, font=2)
  
  n <- which(names(metrics.days)=="MSADPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  mtext(side=3, text="Multi-state SPE", line=1.5, cex=0.8, font=2)
  
  
  
  n <- which(names(metrics.days)=="SSPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  mtext(side=3, text="Single-state T2", line=1.5, cex=0.8, font=2)
  
  n <- which(names(metrics.days)=="MSADPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  mtext(side=3, text="Multi-state T2", line=1.5, cex=0.8, font=2)
  
  filename <- paste0("results/metrics-days-ls-2017-05-28 2017-07-20 ",per.var,"percent.RData")
  load(filename)
  metrics.days <- metrics.days.ls[[days]]
  
  n <- which(names(metrics.days)=="SSPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  mtext(side=2, text="1% Threshold", outer=FALSE, cex=0.8, line=5, font=2)
  
  
  n <- which(names(metrics.days)=="MSADPCA-MT-SPE")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  
  
  
  n <- which(names(metrics.days)=="SSPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
  
  
  
  n <- which(names(metrics.days)=="MSADPCA-MT-T2")
  plot.test.per.thresh(metric.ls = metrics.days, 
                       metric.n=n, y.lim = 20, type="l", date.range=dates)
}
dev.off()








##### Other



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



library(xts)
# Compare 99 and 90%
load("results/metrics-days-ls-2017-02-26 2017-04-03 90percent.RData")
dates <- "2017-03-19/2017-03-25"
par(mfrow=c(2,4))
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
      adj=0.2, line=1.5)

load("results/metrics-days-ls-2017-02-26 2017-04-03 99percent.RData")
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
mtext(side=3, text="99% Variance, MSAD-PCA MT", outer=TRUE, cex=0.8, 
      adj=0.90, line=1.5)
# dev.off()







#### Percent variance vs SS/MS #####
## BR ##
# par(mfcol=c(3,2))
label.n <- 1
label <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")
par(mfcol=c(3,2))
# layout(mat=matrix(1:6, nrow=3, byrow=FALSE))
for(state in c("SS", "MS")) {
  for(per.var in c(80,90,99)) {
    filename <- paste0("results/results-days-ls-2017-02-26 2017-04-03 ",per.var,"percent.RData")
    load(filename)
    allData <- rawData[paste(range(index(results.days.ls[[length(results.days.ls)]][[1]])), collapse ="/")]
    for(day in 1:length(results.days.ls)) {
      names(results.days.ls[[day]]) <- gsub("SSPCA","SSADPCA", names(results.days.ls[[day]]))
    }

    col.n <- which(colnames(allData) == "BIO_2\\LEVEL\\PROCESS_VALUE")
    col.n <- which(colnames(allData) %in% c("BIO_1\\LEVEL\\PROCESS_VALUE",
                                            "BIO_2\\LEVEL\\PROCESS_VALUE",
                                            "BIO_2\\DO\\PROCESS_VALUE",
                                            "BIO_BLOWER_1\\FLOW\\PROCESS_VALUE",
                                            "BIO_BLOWER_2\\FLOW\\PROCESS_VALUE",
                                            "RAS_TROUGH\\TSS\\PROCESS_VALUE"))
    # col.n <- which(colnames(allData) %in% unlist(sapply(varsBR, function(x) grep("PROCESS_VALUE", x, value=TRUE))))
    plotdata <- allData
    colnames(plotdata) <- gsub("_", " ", 
                               gsub("[\\]", " ", 
                                    gsub("[\\]PROCESS_VALUE", "", 
                                         colnames(allData))))
    if(label.n ==length(label)) {
      plot.test.process.alarms(allData=plotdata, col.n=col.n[label.n], 
                               results.days.ls=results.days.ls, 
                               subsys = "BR", 
                               state = state, 
                               multiple=FALSE, 
                               legend.l=TRUE)
    } else {
      plot.test.process.alarms(allData=plotdata, col.n=col.n[label.n], 
                               results.days.ls=results.days.ls, 
                               subsys = "BR", 
                               state = state, 
                               multiple=FALSE, 
                               legend.l=FALSE)
    }
    
    mtext(label[label.n], side=3, adj=0, line=-1, cex=0.8)
    label.n <- label.n + 1
    
  }
}




##### Plot of ALL Alarms #####
library(xts)
sys.ls <- list() # List of SBR and MBR results
all.files <- list.files(path="results", pattern = c("results"), full.names = TRUE)
all.files <- all.files[-grep("2017-02-25 2017-04-03",all.files)]
for(subsys in c("BR", "MT")) {
  config.ls <- list () # List of each PCA config: 99% 1% 99% 10%, etc.
  for(per.var in c(99,90,80)) {
    for(alpha in c(0.01, 0.1)) {
      files <- all.files[intersect(grep(paste0(alpha*100,"alpha"), all.files),
                                   grep(per.var, all.files))]
      files.ls <- list()
      if(length(files)>0) {
        for(f in 1:length(files)) {
          load(files[f])
          metric.state.ls <- list()
          for(day in 1:length(results.days.ls)) {
            data.ls <- results.days.ls[[day]] # 1 = One day
            names(data.ls) <- gsub("SSPCA","SSADPCA", names(data.ls))
            results.ls <- list()
            for(metric in c("SPE", "T2")) {
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
        files.ls <- lapply(1:4, function(n) do.call("rbind", 
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


