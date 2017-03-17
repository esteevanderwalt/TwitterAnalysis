suppressMessages(library(lubridate))

color.names <- function(d) {
  # get RGB components of d and convert to data frame
  z2 <- as.data.frame(t(col2rgb(d)))
  
  # get RGB components of standard colors and convert them to data frame
  z <- as.data.frame(t(sapply(colors(distinct=T),col2rgb)))
  colnames(z) <- colnames(z2)
  z$name <- rownames(z)
  
  # EDIT: original answer used 'merge', which messed up the order
  z2 %>% left_join(z) %>% select(name) 
  
  return(z2)
}

#convert factors to continuous strings
cleanup.factors <- function(data) {
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)
  return(data)
}

#convert all chr to int
cleanup.chr <- function(data) {
  for (i in colnames(data)) {
    #print(typeof(data[,i]))
    if(i != "CLASS" && is.character(data[,i])){
      data[,i] <- as.numeric(data[,i])
    }
  }
  return(data)
}

#convert NA values
#this function is not working - needs some work
cleanup.nulls <- function(data) {
  #df <- sapply(data, is.character)
  #data[is.na(df)] <- " "
  df <- sapply(data, is.numeric)
  data[is.na(df)] <- 0
  return(data)
}

#cleanup of original Twitter dataset
cleanup.Twitter <- function(data) {
  
  b <- 29
  
  #unique variables/nzv which will be ignored <- drop columns
  #ID
  #NAME
  #SCREENNAME
  #DESCRIPTION
  #IS_CELEBRITY
  #LAST_TWEET
  data <- data[ , -which(names(data) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]
  
  #dates -> get year, convert to number
  #CREATED
  data$CREATED <- year(ymd_hms(data$CREATED))
  
  #images -> determine if it is a default image or not, convert to binary
  #ORIGINAL_PROFILE_IMAGE
  data$ORIGINAL_PROFILE_IMAGE[is.na(data$ORIGINAL_PROFILE_IMAGE)] <- 'Null'
  data[grep("default", data$ORIGINAL_PROFILE_IMAGE), "ORIGINAL_PROFILE_IMAGE"] <- 1
  data[data$ORIGINAL_PROFILE_IMAGE != 1, "ORIGINAL_PROFILE_IMAGE"] <- 0
  data$ORIGINAL_PROFILE_IMAGE <- as.numeric(data$ORIGINAL_PROFILE_IMAGE)
  #PROFILE_IMAGE
  data$PROFILE_IMAGE[is.na(data$PROFILE_IMAGE)] <- 'Null'
  data[grep("default", data$PROFILE_IMAGE), "PROFILE_IMAGE"] <- 1
  data[data$PROFILE_IMAGE != 1, "PROFILE_IMAGE"] <- 0
  data$PROFILE_IMAGE <- as.numeric(data$PROFILE_IMAGE)
  #BACKGROUND_IMAGE
  data$BACKGROUND_IMAGE[is.na(data$BACKGROUND_IMAGE)] <- 'Null'
  data[grep("themes", data$BACKGROUND_IMAGE), "BACKGROUND_IMAGE"] <- 1
  data[data$BACKGROUND_IMAGE != 1, "BACKGROUND_IMAGE"] <- 0
  data$BACKGROUND_IMAGE <- as.numeric(data$BACKGROUND_IMAGE)

  #continious strings -> get top 29 entries, rest = 'Other', convert to numeric based on rank
  #LOCATION
  data$LOCATION[is.na(data$LOCATION)] <- 'Null'
  d <- data %>% 
    group_by(LOCATION) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  c <- data %>% distinct(LOCATION) %>% summarise(count=n())
  print("1")
  if(c > 30){
    l <- subset(data, !(LOCATION %in% d$LOCATION[1:b]))$LOCATION
    data$LOCATION[data$LOCATION %in% l] <- 'Other'
  }
  #convert to ranked
  print("2")
  r <- data %>% count(LOCATION, sort = TRUE) %>% mutate(rank = row_number(desc(n)))
  data <- left_join(data, r, by=c('LOCATION'='LOCATION')) %>%
    mutate(LOCATION = rank) %>% 
    select(-rank, -n)
  print("3")
  #data$LOCATION <- as.numeric(factor(data$LOCATION))
  #LANGUAGE
  data$LANGUAGE[is.na(data$LANGUAGE)] <- 'Null'
  d <- data %>% 
    group_by(LANGUAGE) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  l <- subset(data, !(LANGUAGE %in% d$LANGUAGE[1:b]))$LANGUAGE
  data$LANGUAGE[data$LANGUAGE %in% l] <- 'Other'
  if(c > 30){
    l <- subset(data, !(LANGUAGE %in% d$LANGUAGE[1:b]))$LANGUAGE
    data$LANGUAGE[data$LANGUAGE %in% l] <- 'Other'
  } 
  print("4")
  #convert to ranked
  r <- data %>% count(LANGUAGE, sort = TRUE) %>% mutate(rank = row_number(desc(n)))
  data <- left_join(data, r, by=c('LANGUAGE'='LANGUAGE')) %>%
    mutate(LANGUAGE = rank) %>% 
    select(-rank, -n)
  #data$LANGUAGE <- as.numeric(factor(data$LANGUAGE))
  #TIMEZONE
  data$TIMEZONE[is.na(data$TIMEZONE)] <- 'Null'
  d <- data %>% 
    group_by(TIMEZONE) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  l <- subset(data, !(TIMEZONE %in% d$TIMEZONE[1:b]))$TIMEZONE
  data$TIMEZONE[data$TIMEZONE %in% l] <- 'Other'
  if(c > 30){
    l <- subset(data, !(TIMEZONE %in% d$TIMEZONE[1:b]))$TIMEZONE
    data$TIMEZONE[data$TIMEZONE %in% l] <- 'Other'
  } 
  #convert to ranked
  r <- data %>% count(TIMEZONE, sort = TRUE) %>% mutate(rank = row_number(desc(n)))
  data <- left_join(data, r, by=c('TIMEZONE'='TIMEZONE')) %>%
    mutate(TIMEZONE = rank) %>% 
    select(-rank, -n)
  #data$TIMEZONE <- as.numeric(factor(data$TIMEZONE))
  #UTC_OFFSET ignore as it is the same as timezone
  data <- data[ , -which(names(data) %in% c("UTC_OFFSET"))]

  #numeric counts -> convert to hundreds
  #FRIENDS_COUNT
  data$FRIENDS_COUNT[is.na(data$FRIENDS_COUNT)] <- 0
  data$FRIENDS_COUNT <- round(data$FRIENDS_COUNT/500)
  #FOLLOWERS_COUNT
  data$FOLLOWERS_COUNT[is.na(data$FOLLOWERS_COUNT)] <- 0
  data$FOLLOWERS_COUNT <- round(data$FOLLOWERS_COUNT/500)
  #STATUS_COUNT 
  data$STATUS_COUNT[is.na(data$STATUS_COUNT)] <- 0
  data$STATUS_COUNT <- round(data$STATUS_COUNT/500)
  #LISTED_COUNT
  data$LISTED_COUNT[is.na(data$LISTED_COUNT)] <- 0
  data$LISTED_COUNT <- round(data$LISTED_COUNT/500)
  
  #binary - dont need to do anything here
  #GEO_ENABLED
  data$GEO_ENABLED[is.na(data$GEO_ENABLED)] <- 0
  #IS_DEFAULT_PROFILE
  data$IS_DEFAULT_PROFILE[is.na(data$IS_DEFAULT_PROFILE)] <- 0
  #IS_DEFAULT_PROFILE_IMAGE 
  data$IS_DEFAULT_PROFILE_IMAGE[is.na(data$IS_DEFAULT_PROFILE_IMAGE)] <- 0
  #IS_BACKGROUND_IMAGE_USED
  data$IS_BACKGROUND_IMAGE_USED[is.na(data$IS_BACKGROUND_IMAGE_USED)] <- 0
  
  #geo -> divide long by 30 and lat by 15 to get 12 bins each
  #LATITUDE 
  data$LATITUDE[is.na(data$LATITUDE)] <- 0
  data$LATITUDE <- round(data$LATITUDE/15)
  #LONGITUDE
  data$LONGITUDE[is.na(data$LONGITUDE)] <- 0
  data$LONGITUDE <- round(data$LONGITUDE/30)

  #colors -> convert to name, get top 29, rest is other, convert to numeric based on rank
  #PROFILE_TEXT_COLOR
  data$PROFILE_TEXT_COLOR[is.na(data$PROFILE_TEXT_COLOR)] <- 'Null'
  d <- data %>% 
    group_by(PROFILE_TEXT_COLOR) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  l <- subset(data, !(PROFILE_TEXT_COLOR %in% d$PROFILE_TEXT_COLOR[1:b]))$PROFILE_TEXT_COLOR
  data$PROFILE_TEXT_COLOR[data$PROFILE_TEXT_COLOR %in% l] <- 'Other'
  rm(d, l) 
  #convert to ranked
  r <- data %>% count(PROFILE_TEXT_COLOR, sort = TRUE) %>% mutate(rank = row_number(desc(n)))
  data <- left_join(data, r, by=c('PROFILE_TEXT_COLOR'='PROFILE_TEXT_COLOR')) %>%
    mutate(PROFILE_TEXT_COLOR = rank) %>% 
    select(-rank, -n)
  #data$PROFILE_TEXT_COLOR <- as.numeric(factor(data$PROFILE_TEXT_COLOR))
  #PROFILE_BG_COLOR
  data$PROFILE_BG_COLOR[is.na(data$PROFILE_BG_COLOR)] <- 'Null'
  d <- data %>% 
    group_by(PROFILE_BG_COLOR) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  l <- subset(data, !(PROFILE_BG_COLOR %in% d$PROFILE_BG_COLOR[1:b]))$PROFILE_BG_COLOR
  data$PROFILE_BG_COLOR[data$PROFILE_BG_COLOR %in% l] <- 'Other'
  rm(d, l)
  #convert to ranked
  r <- data %>% count(PROFILE_BG_COLOR, sort = TRUE) %>% mutate(rank = row_number(desc(n)))
  data <- left_join(data, r, by=c('PROFILE_BG_COLOR'='PROFILE_BG_COLOR')) %>%
    mutate(PROFILE_BG_COLOR = rank) %>% 
    select(-rank, -n)
  #data$PROFILE_BG_COLOR <- as.numeric(factor(data$PROFILE_BG_COLOR))
  
  #convert class to factor
  #data$CLASS <- as.factor(data$CLASS)
  
  return(data)
}