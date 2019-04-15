# Create results folder
dir.create(file.path(paste0(wd,"\\results\\")), showWarnings = FALSE)

# Plot to check for normal operating conditions (i.e., good training data)
graph_rawData(rawData, paste0("results/",keyword), trueFaultTime = trueFaultTime)

# Plot of testing data
graph_alarmData(data = alarmDataSS[,c(-grep("PROCESS_VALUE.1", colnames(alarmDataSS)))],
                keyword = paste0("results/",keyword,"_alarmDataSS"),
                # trueFaultTime = trueFaultTime,
                trueAlarmTime = trueAlarmTime)
graph_alarmData(data = alarmDataBR[,c(-grep("PROCESS_VALUE.1", colnames(alarmDataBR)))],
              keyword = paste0("results/",keyword,"_alarmDataBR"),
              # trueFaultTime = trueFaultTime,
              trueAlarmTime = trueAlarmTime)
graph_alarmData(data = alarmDataMT[,c(-grep("PROCESS_VALUE.1", colnames(alarmDataMT)))],
              keyword = paste0("results/",keyword,"_alarmDataMT"),
              # trueFaultTime = trueFaultTime,
              trueAlarmTime = trueAlarmTime)

# Plot of alarm data
daterange <- c(as.POSIXct(paste(testingDay,"00:00:00 UTC")), as.POSIXct(paste(testingDay+1,"00:00:00 UTC")))
objectlist <- list(alarmDataSS, alarmDataBR, alarmDataMT)
titlelist <- c("SS Alarms", "BR Alarms", "MT Alarms")
filename <- paste0("results/",keyword, " alarmPlots ",testingDay)
plot.alarm.data <- function(){
  par(mfrow=c(3,1), mar = c(2.1, 3.1, 2.1, 2.1))
  for(i in 1:3) {
    a <- which(index(objectlist[[i]]) > daterange[1])
    a <- a[which(index(objectlist[[i]])[a] < daterange[2])]
    alarmCol <- which(colnames(objectlist[[i]]) == "Alarm")
    # Using zoo object because problem in plot.xts
    plot(as.zoo(objectlist[[i]][a,alarmCol][!is.na(objectlist[[i]][a,alarmCol])]),
         yaxt="n",
         ylab="",
         ylim=c(0,3),
         pch=19)
    abline(v=trueFaultTime, col="blue")
    title(titlelist[i])
    axis(2, at=c(0,1,2,3), labels = c("No","SPE","T2","Both"))
  }
}
svg(filename = paste(filename, ".svg", sep=""), width = 8, height = 6, family = "serif")
plot.alarm.data()
dev.off()
# plot.alarm.data()

