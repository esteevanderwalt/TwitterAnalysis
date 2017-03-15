#convert factors to continuous strings
cleanup.factors <- function(data) {
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)
  return(data)
}

#convert NA values
cleanup.nulls <- function(data) {
  df <- sapply(data, as.character)
  data[is.na(df)] <- " "
  df <- sapply(data, as.numeric)
  data[is.na(df)] <- 0
  return(data)
}

