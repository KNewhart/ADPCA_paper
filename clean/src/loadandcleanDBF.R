# Import and Prepare Data
# dataLocation <- "MP SB-MBR Data\\"
# testingDay <- as.Date("2017 08 31", format = "%Y %m %d")
# nDays <- 30
# rawData <- loadandcleanDBF(dataLocation, testingDay, nDays)

loadandcleanDBF <- function (dataLocation) {

  # Compile raw data from data files (no column names, extraneous empty columns)
  rawData <- loadDBF(dataLocation)

  # Read in data labels from 'Tagname' file
  columnNames <- read.dbf(list.files(path=dataLocation, pattern="Tagname", full.names = TRUE)[1])

  # Run Cleaner function (rename columns, remove extraneous columns)
  rawData <- cleanDBF(rawData, columnNames)

  return(rawData)
}
