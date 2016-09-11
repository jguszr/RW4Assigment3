

# Helper function used to manipulate the dataframe based on the state, and the 
#desired output.
helper <- function(data, colNum, state) {
  stateSubset <- data[data[, 7]==state, ]
  outcomeArr <- stateSubset[, colNum]
  return(stateSubset[which(outcomeArr == min(outcomeArr, na.rm=T)), 2])
}

# Assert function used to validate the informed inputs.
# used just to make the code more readable.
assertInputs <- function(data,state,outcome) {

  if (!state %in% data$State) {
    return(FALSE)
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    return(FALSE)
  }
  
  return(TRUE)
}

# Another helper function used to make the main function (best) more readable
# it returns the founded hospital name.
assertHospitalName<- function(outcome,dt,state) {
  
  hosp_name <- "invalid"
  if(outcome == "heart attack") {
    hosp_name <- helper(dt, 11, state)
  } else if(outcome == "heart failure") {
    hosp_name <- helper(dt, 17, state)
  } else {
    hosp_name <- helper(dt, 23, state)
  }
  
  return(hosp_name)  
}

# Main function for the best exercise, it reads the indicated CVS, prepares it, assert the paramenters
# and filter the outcomes based on the inputed values.
best<- function(state,outcome) {
  
  dt <- read.csv("outcome-of-care-measures.csv",header=TRUE) 
  dt[, 11] <- as.numeric(dt[, 11]) # heart attack
  dt[, 17] <- as.numeric(dt[, 17]) # heart failure
  dt[, 23] <- as.numeric(dt[, 23]) # pneumonia
  
  if (assertInputs(dt,state,outcome)==FALSE) {
    print("Invalid Stuff ! check your input parameters ")
    stop("invalid Stuff")
    return("Not valid stuff")
  }

  return(assertHospitalName(outcome,dt,state))

}

best2 <- function(state, outcome) {
  
  ## Read the outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% unique(dat[, 7])) {
    stop("invalid state")
  }
  switch(outcome, `heart attack` = {
    col = 11
  }, `heart failure` = {
    col = 17
  }, pneumonia = {
    col = 23
  }, stop("invalid outcome"))
  ## Return hospital name in that state with lowest 30-day death rate
  df = dat[dat$State == state, c(2, col)]
  df[which.min(df[, 2]), 1]
}

