best <- function(state, outcome){
  #*****reading outcome data. Using file.choose() to allow user locate file******
  if(!exists("outcome_csv", 1)){ #checking if data exist in Global Environment. Avoiding multiple read.csv executions
    outcome_csv <<- read.csv(file.choose(), colClasses = 'character') # setting to character to load data faster.
  }
  #*****function adjustments and variable definitions*****
  #Extracting vector with all state abreviations (no duplicates)
  states_abr <- unique(outcome_csv[, 7])
  #defining outcome vector with possible values and their column indexes in the .csv file
  df_outcome <- data.frame(outcome_name = c("heart attack", "heart failure", "pneumonia"), index = c(11, 17, 23))
  #in case user input state abbreviations in lowercase.
  up_state <- toupper(state) 
  
  #*****verifying state and outcome validity*****
  #Validating if 'state' value exist in states_abr vector and if outcome exist in outcome_vector.
  ## If they don't exist, function will error out.
  if(!up_state %in% states_abr){
    stop('invalid state. Ensure use of Uppercase.') #Execution will stop due to wrong state input.
  }
  else if(!outcome %in% df_outcome$outcome_name){
    stop('invalid outcome.') #Execution will stop due to wrong outcome input.
  }
  #*****Finding best hospital with state and outcome values*****
  #1. Extracting from outcome_csv all hospitals matching input values (subseting by criteria)
  sub_hospframe <- subset(outcome_csv, State == up_state, select = c(2, 7, df_outcome[df_outcome$outcome_name == outcome, "index"]))
  #2. Removing NAs (requires data type coersion for Rate values)
  sub_hospframe[, 3] <- as.numeric(sub_hospframe[, 3]) #coercing Rates to numeric data type
  sub_hospframe <- na.omit(sub_hospframe) #DF with NAs removed
  #3. Finding min value of rate column, extracting hospital names with rate value = min and handling ties (if any)
  min_rate_value <- min(sub_hospframe[, 3])
  sub_hospname <- subset(sub_hospframe, sub_hospframe[, 3] == min_rate_value)#subsetting with min value as criteria
  #returning Hospital Name, State and Mortality rate (lowest)
  return(sub_hospname)
}