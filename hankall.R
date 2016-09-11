
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


rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- na.omit(data)
  states = unique(data[, 7])
  col = assertColNumber(outcome)
  data[, col] = as.numeric(data[, col])
  data = na.omit(data[, c(2, 7, col)] ) 

  
  rank_in_state <- function(state) {
    df = data[data[, 2] == state, ]
    hospCount = nrow(df)
    switch(num, best = {
      num = 1
    }, worst = {
      num = hospCount
    })
    if (num > hospCount) {
      result = NA
    }
    c(df[order(df[, 3], df[, 1]), ][num, 1], state)
  }
  
  
  output = do.call(rbind, lapply(states, rank_in_state))
  output = output[order(output[, 2]), ]
  rownames(output) = output[, 2]
  colnames(output) = c("hospital", "state")
  data.frame(output)
}