rankhospital <- function(state, outcome, num = "best") {
  
  options(warn = -1)
  
  datas <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  names(datas)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
  
  hospitals <- datas[, c(2, 7, 11, 17, 23)]
  
  hospitals[, outcome] <- as.numeric(hospitals[, outcome])
  
  listing <- hospitals[hospitals$State == state, ]
  listing <- listing[, c("Hospital.Name", outcome)]
  listing <- listing[complete.cases(listing), ]
  listing <- listing[order(listing[2], listing[1]),]
  
  hosp <- unlist(listing$Hospital.Name)
  
  if (num == "best")
    hosp[1]
  else if (num == "worst")
    hosp[length(hosp)]
  else if (num > length(hosp) || num < 0)
    NA
  else if (class(num) == "numeric")
    hosp[num]
  else
    NA
  
  
}