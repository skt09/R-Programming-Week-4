best <- function(state, outcome) {
  options(warn=-1)
  
  datas <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  names(datas)[c(11, 17, 23)] <- c("heart attack", "heart failure", "pneumonia")
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  states <- unique(datas$State)
  
  if(!(state %in% states))
    stop("invalid state")
  if(!(outcome %in% outcomes))
    stop("invalid outcome")
  
  hospitals <- datas[, c(2, 7, 11, 17, 23)]
  
  hospitals[, outcome] <- as.numeric(hospitals[, outcome])
  
  listing <- hospitals[hospitals$State == state, ]
  listing <- listing[, c("Hospital.Name", outcome)]
  listing <- listing[complete.cases(listing), ]
  listing <- listing[which.min(unlist(listing[outcome])),]
  
  hosp <- unlist(listing$Hospital.Name)
  
  hosp <- sort(hosp)
  
  hosp[1]
  
}