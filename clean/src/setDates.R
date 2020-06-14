# Parse dates
# Take the start and end date and turn them into integers that can be iterated through

setDates <- function(startDay,endDay) {

  # Set final day/file as the previous day
  finalMonth <- as.numeric(format(endDay, "%m"))
  finalDay <- as.numeric(format(endDay, "%d"))
  finalYear <- as.numeric(format(endDay, "%Y"))

  # Set initial day/file as 10 days prior
  initialMonth <- as.numeric(format(startDay, "%m"))
  initialDay <- as.numeric(format(startDay, "%d"))
  initialYear <- as.numeric(format(startDay, "%Y"))

  # The 'Current' Date is the date actively being copied
  # Set as the first date
  currentMonth <- as.numeric(format(startDay, "%m"))
  currentDay <- as.numeric(format(startDay, "%d"))
  currentYear <- as.numeric(format(startDay, "%Y"))

  dates <- list ("currentMonth" = currentMonth,
                 "currentDay" = currentDay,
                 "currentYear" = currentYear,
                 "initialMonth" = initialMonth,
                 "initialDay" = initialDay,
                 "initialYear" = initialYear,
                 "finalMonth" = finalMonth,
                 "finalDay" = finalDay,
                 "finalYear" = finalYear)
  return(dates)
}
