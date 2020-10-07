best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!(state %in% outcomeData$State)) stop("invalid state")
  if(!(outcome %in% c("heart attack","heart failure", "pneumonia"))) stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  if(outcome == "heart attack"){
    result = outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcomeData$Hospital.Name),]$Hospital.Name[1]
  } else if (outcome == "heart failure") {
    result = outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcomeData$Hospital.Name),]$Hospital.Name[1]
  } else {
    result = outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcomeData$Hospital.Name),]$Hospital.Name[1]
  }
  
result
  
}
