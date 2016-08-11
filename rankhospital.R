rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if ((state %in% out$State)==F) stop('invalid state!!')
  if ((outcome %in% c("heart attack", "heart failure", "pneumonia"))==F)
    stop('invalid outcome!!')
  
  ## Return hospital name in that state with the given rank
  if (outcome=="heart attack")  {clmn <- 11}
  if (outcome=="heart failure") {clmn <- 17}
  if (outcome=="pneumonia")     {clmn <- 23}
      splt <- split(out, out$State)[[state]]
      numer <- as.numeric(splt[,clmn])
      hos_names <- c()
  if (num == "best") {num <- 1}
  if (num == "worst") {num <- which(tail(sort(numer),1)==sort(numer))}   
  if (num > dim(out)[1]){stop('num more than total no. of obs.')}
  for (i in 1:num){
      min <- min(numer, na.rm = TRUE)
      index <- which(min == numer)
      hos_names <- c(hos_names, sort(splt[,2][index]))
      numer[c(which(min==numer))] <- c(rep(NA, length(min)))
  }
      
  if (length(hos_names) > num) {hos_names <- hos_names[c(1:num)]}
  tail(hos_names,1)
}