trueDetectionRate <- function(actual, model) {
  actual.oc <- which(actual=="OC")
  actual.t <- index(actual[actual.oc])
  model.oc <- which(model=="OC")
  model.t <- index(model[model.oc])
  tdr <- length(which(actual.t %in% model.t))/length(actual.t)
  return(tdr)
}

falseDetectionRate <- function(actual, model) {
  actual.ic <- which(actual=="IC")
  actual.t <- index(actual[actual.ic])
  model.oc <- which(model=="OC")
  model.t <- index(model[model.oc])
  fdr <- length(which(actual.t %in% model.t))/length(actual.t)
  return(fdr)
}