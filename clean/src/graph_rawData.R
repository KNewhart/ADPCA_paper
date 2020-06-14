# Graph raw data
packageLoad <- function(packName){ #packName - package name as a character string, e.g. "quantmod"
  if(!require(packName,character.only = TRUE)){ #If the package is not available, install it
    install.packages(packName,dependencies=TRUE,repos="http://cran.r-project.org")
  }
  library(packName, character.only = TRUE) # load package
}

graph_rawData <- function (data, keyword="", trueFaultTime = NULL, fault.interval = NULL, output=TRUE) {
  packageLoad("scales")
  cols <- grep("PROCESS_VALUE", colnames(data))
  data <- data[,cols]
  colnames(data) <- sapply(colnames(data), function(x) unlist(strsplit(x, "PROCESS_VALUE"))[1])
  colnames(data) <- gsub("_"," ",colnames(data))
  colnames(data) <- gsub("\\"," ", colnames(data), fixed=TRUE)
  n <- ncol(data)
  dates <- index(data)
  r <- as.POSIXct(range(dates), format = "%Y-%m-%d %H:%M:%S")
  r.hours <- as.numeric(difftime(r[2], r[1], units = "hours"))
  if (keyword=="") {
    filename <- paste("allGraphs",as.Date(r[1]), as.Date(r[2]))
    } else {
      filename <- paste(keyword, "allGraphs",as.Date(r[1]), as.Date(r[2]))
    }



  plot.it <- function() {
    # par(mfrow=c(n,1), mar = c(2.1, 3.1, 4.1, 2.1))
    par(mar = c(2.1, 2.1, 2.1, 0.6), family="serif", cex=0.9
        # , mfrow=c(ncol(data),1)
        )
    if(output) par(mar = c(2.1, 2.1, 2.1, 0.6), family="serif", cex=0.9, mfrow=c(ncol(data),1))

    for (i in 1:n) {
      plot(x = dates,
           y = data[,i],
           xaxt="n",
           xlab = "",
           ylab = "",
           main = colnames(data)[i],
           pch=1,
           col = "black", 
           cex=.5)
        # line.ewma <- ewma(data[,i])
        # line(x = index(line.ewma),
        #      y = line.ewma)
        if (r.hours > 24) {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
        } else {
          axis.POSIXct(1, at = seq(r[1], r[2], by = "hours"), cex.axis = 1, format = "%H:%M")
        }
        # axis.POSIXct(1, at = seq(r[1], r[2], by = "days"), cex.axis = 1, format = "%m/%d")
        if (!is.null(trueFaultTime)) abline(v=trueFaultTime, col="blue")
        if (!is.null(fault.interval)) {
          for(i in 1:length(fault.interval)) {
            abline(v=fault.interval[i]@start, col="red")
            abline(v=fault.interval[i]@start+fault.interval[i]@.Data,col="red")
          }
        }
      }
    }

  # png(file = paste(filename, ".png", sep=""), units = "in", res = 96, width = 11, height =n*3)
  # pdf(file = paste(filename,".pdf", sep=''),
  #     width = 30,
  #     height = (n)*2)
  # svg(filename = paste(filename, ".svg", sep=""), width = 8, height =n*2, family = "serif")
  if(output) png(file = paste(filename, ".png", sep=""), units = "in", res = 300, width = 8, height = ncol(data)*2)
  plot.it()
  if(output) dev.off()
  # plot.it() # Margins too large to plot
}
