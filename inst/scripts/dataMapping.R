# added to help maintain column naming consistency
library(dplyr)

mapHallOfFame <- function(df) {
  df <- df %>% rename(yearID = yearid)
  
  return(df)
}

mapHomeGames <- function(df) {
  colnames(df) <- gsub("(.*)key$", "\\1.key", colnames(df))
  colnames(df) <- gsub("^span(.*)", "span.\\1", colnames(df))
  
  return(df)
}

mapParks <- function(df) {
  colnames(df) <- gsub("^park(.*)", "park.\\1", colnames(df))
  df <- df %>% select(-ID)
  
  return(df)
}

mapPeople <- function(df) {
  df <- df %>% select(-ID)
  
  return(df)
}
