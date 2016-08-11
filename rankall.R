rankall <- function(outcome, num = "best") {
  ## Read outcome data
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if ((state %in% out$State)==F) stop('invalid state!!')
  if ((outcome %in% c("heart attack", "heart failure", "pneumonia"))==F)
    stop('invalid outcome!!')
  
  ## For each state, find the hospital of the given rank
  source("rankhospital.R")
  list <- c()
  for (state in unique(sort(out$State))) {
  hospital <- rankhospital(state,outcome,num)
  list <- rbind(list, cbind(hospital, state))
  }
  ## Return a data frame with the hospital names and the (abbreviated) state name
  list
}