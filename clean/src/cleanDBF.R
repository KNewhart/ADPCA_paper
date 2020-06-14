# Clean .DBF

cleanDBF <- function(x,y = NULL) {
  # Merge the date and time columns
  x$Date <- as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S")
  # Remove the time column
  x$Time <- NULL
  # Remove 'Millitm' column
  x$Millitm <- NULL
  # Remove 'Marker' column
  x$Marker <- NULL
  # Remove 'Sts_XX' columns,
  lastCol <- colnames(x)[ncol(x)]
  n <- as.numeric(substr(lastCol,5,6))
  for (i in 0:n) {
    if (i < 10) {
      name <- paste("Sts_0",i,sep = '')
    }
    else {
      name <- paste("Sts_",i,sep = '')
    }
    x[[name]] <- NULL
  }

  # Remove rows that are only 0's
  # First, subset all of the numeric data in order to use the 'rowSums' function
  numericData <- subset(x[,2:ncol(x)])
  # Second, the sum of the state functions  = 4, therefore greater than 4 is equivalent to all zeros
  x <- x[rowSums(numericData[,-1]) > 4, ]

  if (!is.null(y)) { # If the column names have been read in
    colnames(x)[2:ncol(x)] <- as.character(y[1:nrow(y),1]) # Name columns from 'Tagname' file
  }

  x <- subset(x, !duplicated(Date)) # Check for duplicates in date column

  # Check for NAs
  if (anyNA(x)) {
    x <- na.omit(x)
  }

  return(x)
}
