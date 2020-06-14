control.limits <- function(test.data) {
  limits.table <- read.csv("src/control-limits.csv", stringsAsFactors = FALSE)
  results.table <- matrix(data=NA, nrow=nrow(test.data), ncol=length(limits.table[,1]))
  
  for(i in 1:length(limits.table[,1])) {
    var.col <- which(colnames(test.data) %in% limits.table[i,1])
    data <- test.data[,var.col]
    lcl <- limits.table[i,ncol(limits.table)-1]
    ucl <- limits.table[i,ncol(limits.table)]
    if(nchar(limits.table[i,2])>0) { # If there are conditional statements
      cond.col <- which(colnames(test.data) %in% limits.table[i,2])
      cond.obs <- which(test.data[,cond.col] == limits.table[i,3])
      if(nchar(limits.table[i,4])>0) {
        cond.col <- which(colnames(test.data) %in% limits.table[i,4])
        cond.obs <- cond.obs[which(cond.obs %in% which(test.data[,cond.col] == limits.table[i,5]))]
      } else {}
      data <- data[cond.obs,]
    } else {}
    oc.obs <- vector()
    if(!is.na(lcl)) oc.obs <- c(oc.obs, which(data<lcl))
    if(!is.na(ucl)) oc.obs <- c(oc.obs, which(data>ucl))
    if(length(oc.obs)>0) results.table[oc.obs,i] <- "OC"
  }
  
  for(i in 1:length(unique(limits.table[,1]))) {
    vars2combine <- which(limits.table[,1]==unique(limits.table[,1])[i])
    concat <- matrix(data="IC", nrow=nrow(test.data), ncol=1)
    concat[which(apply(data.frame(results.table[,vars2combine]), 1,
                       function(x) length(which(is.na(x))))<length(vars2combine)),] <- "OC"
    if(i==1) test.limits <- xts(concat, 
                                order.by=index(test.data))
    if(i>1) test.limits <- cbind(test.limits, xts(concat, 
                                                  order.by=index(test.data)))
  }
  
  colnames(test.limits) <- unique(limits.table[,1])
  
  
  return(test.limits) # xts object test.data with IC (0) or OC (1) labels
}