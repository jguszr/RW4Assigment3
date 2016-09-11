
helper <- function(data, colNum, state) {
  stateSubset <- data[data[, 7]==state, ]
  outcomeArr <- stateSubset[, colNum]
  return(stateSubset[which(outcomeArr == min(outcomeArr, na.rm=T)), 2])
}


assertInputs <- function(data,state,outcome) {

  if (!state %in% data$State) {
    return(FALSE)
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    return(FALSE)
  }
  
  return(TRUE)
}



best<- function(state,outcome) {
  
  hosp_name <- "Not Found"

  dt <- read.csv("outcome-of-care-measures.csv",header=TRUE) 
  dt[, 11] <- as.numeric(dt[, 11]) # heart attack
  dt[, 17] <- as.numeric(dt[, 17]) # heart failure
  dt[, 23] <- as.numeric(dt[, 23]) # pneumonia
  
  if (assertInputs(dt,state,outcome)==FALSE) {
    print("Invalid Stuff ! check your input parameters ")
    stop("invalid Stuff")
    return("Not valid stuff")
  }

  if(outcome == "heart attack") {
    hosp_name <- helper(dt, 11, state)
  } else if(outcome == "heart failure") {
    hosp_name <- helper(dt, 17, state)
  } else {
    hosp_name <- helper(dt, 23, state)
  }
  
  return(hosp_name[1])

}