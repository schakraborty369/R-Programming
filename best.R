best <- function(state, outcome) {
  
  ## Read outcome data
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if ((state %in% out$State)==F) stop('invalid state!!')
  if ((outcome %in% c("heart attack", "heart failure", "pneumonia"))==F)
     stop('invalid outcome!!')
  
  ## Return hospital name in that state with lowest 30-day death rate
  if (outcome=="heart attack")  {clmn <- 11}
  if (outcome=="heart failure") {clmn <- 17}
  if (outcome=="pneumonia")     {clmn <- 23}
  
  splt <- split(out, out$State)[[state]]
  min <- min(as.numeric(splt[,clmn]), na.rm = TRUE)
  index <- which(min == as.numeric(splt[,clmn]))
  sort(splt[,2][index])
}