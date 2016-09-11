
# Assert the informed state input, used to make the core function more readable
assertInputs <- function(data,state) {
  
  if (!state %in% data$State) {
    return(FALSE)
  } 
  return(TRUE)
}

# assert the column number based on the informed outcome parameter or stop the function 
# also used to make the hankHospital more readable 
assertColNumber <- function(outcome) {
  switch(outcome, "heart attack" = {
    col = 11
  }, "heart failure" = {
    col = 17
  }, "pneumonia" = {
    col = 23
  }, stop("invalid outcome"))
  
  return(col)
  
}

# the hankhospital function takes the state and outcome parameters to act as "key" filters 
# and aftwords it uses the num parameter to check the hanking of a hospital.
rankhospital <- function(state, outcome, num = "best") {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",header = TRUE)
  
  if(assertInputs(data,state) == FALSE) {
    stop("invalid state")
  }
  
  col <- assertColNumber(outcome)
  
  data[, col] <- as.numeric(data[, col])
  df <- na.omit(data[data[, 7] == state, c(2, col)])
  hospitals = nrow(df)
  
  switch(num, best = {
    num <- 1
  }, worst = {
    num <- hospitals
  })
  
  if (num > hospitals) {
    return(NA)
  }
  
  return(df[order(df[, 2], df[, 1]), ][num, 1])
}


