# Load all original data files
# rawData <- loadDBF(dataLocation, testingDay, nDays)

# This function takes in the location of the 1-min SB-MBR data files
# (where "LogAllData" is located including fial backslashes),
# the last day to be compiled, and the number of previous days to include

loadDBF <- function(dataLocation) {
  # Check if package is included already, if not then load
  library("foreign")
  
  file.names <- list.files(path=dataLocation, pattern="Wide")
  endDay <- as.Date(substr(tail(file.names,n=1),1,10), format="%Y %m %d")
  nDays <- as.numeric(endDay - as.Date(substr(head(file.names,n=1),1,10), format="%Y %m %d"))

  # Create an object that includes numeric day, month, and year values to iterate through
  dates <- setDates((endDay - nDays), endDay)

  # Create empty dataframe
  compiledData <- list()

  # Loop through all files within the date range
  # Loop through years
  while (dates$currentYear <= dates$finalYear) {
    # Loop through months of the year
    while (((dates$currentMonth <= dates$finalMonth) && (dates$currentYear == dates$finalYear)) || ((dates$currentMonth <= 12) && (dates$currentYear < dates$finalYear))) {
      # Loop through days of the month
      while (((dates$currentDay <= 31) && (dates$currentMonth != dates$finalMonth)) || ((dates$currentDay <= dates$finalDay) && (dates$currentMonth == dates$finalMonth))) {

        # Create a string from the current date to access file
        dateString <- paste(dates$currentYear,sprintf("%02d",dates$currentMonth),sprintf("%02d",dates$currentDay),sep=' ')

        ### Check to see if file exists:
        if(length(which(substr(file.names,1,10)==dateString))<1) {

          # February special case
          if((dates$currentDay >= 29) && (dates$currentMonth == 2)) {
            break
          }

          # If the file does not exist, and the date is the 31st...
          if (dates$currentDay > 31) {
            # ... the end of the month has been reached, so break from the daily 'while' loop
            break
          } else { # If the file does not exist, and it is not the end of the month...
            #... there is a missing file. Progress to the next day.
            if (!file.exists(paste(dataLocation,dateString," 0000 (Wide).DBF",sep=''))) {
              dates$currentDay <- dates$currentDay + 1
              next
            } else {}

          }
        } else {} # File exists, therefore end the if statement





        ### Read in raw data from 'Wide' file:
        # Create an empty dataframe for the raw, imported data
        rawData <- NULL
        file.match <- paste(dataLocation,file.names[which(substr(file.names,1,10)==dateString)], sep='')
        for(file in file.match) {
          # Attempt to open file. If file can be opened, write to dataframe.
          tryCatch(rawData <- read.dbf(file),error=function(e){})
          # If the file could not be opened, the dataframe is still empty.
          if (length(rawData) == 0) {
            # Since file could not be opened, move onto the next day
            dates$currentDay <- dates$currentDay + 1
            next
          }
          
          ### Compile raw data from previous days:
          compiledData[[length(compiledData)+1]] <- rawData
        }
        
        ### Progress through loop:
        # Go to next day in the month
        dates$currentDay <- dates$currentDay + 1
      }
      # Go to next month in the year
      dates$currentMonth <- dates$currentMonth + 1
      # Go to the first day of the next month
      dates$currentDay <- 1
    }
    # Go to the next year
    dates$currentYear <- dates$currentYear + 1
    # Reset the month to the beginning of the next year
    dates$currentMonth <- 1
  }
  
  allData <- do.call("rbind", compiledData)

  return(allData)

}
