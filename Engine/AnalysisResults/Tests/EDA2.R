suppressMessages(library(dplyr))
suppressMessages(library(RODBC))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(doParallel))
suppressMessages(library(FSelector))

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_CELEBRITY, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, CLASS from twitter.zz_full_set") )
close(myconn)

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

#get unique values in set
#rapply(data.original,function(x)length(unique(x)))

#' ######################################
#' ###  Cleanup and preprocessing
#' ######################################
#+ clean_preprocess
#change factors to characters
data.full <- cleanup.factors(data.full)
#detach("package:dplyr", unload=TRUE)
data.clean <- cleanup.Twitter(data.full)

#' ######################################
#' ### Prepare Datasets with dummy vars
#' ######################################
#+ prepare
#rapply(data.full,function(x)length(unique(x)))
#rapply(data.full,function(x)sum(is.na(x)))
#dataset C - remove columns not used by fake accounts
data.cleanB <- data.clean[ , -which(names(data.clean) %in% c("CREATED","LOCATION"))]
data.cleanB2 <- data.clean[ , -which(names(data.clean) %in% c("CREATED","LOCATION","ORIGINAL_PROFILE_IMAGE","BACKGROUND_IMAGE","PROFILE_TEXT_COLOR","PROFILE_BG_COLOR"))]
data.cleanC <- data.clean[ , -which(names(data.clean) %in% c("CREATED","ORIGINAL_PROFILE_IMAGE","BACKGROUND_IMAGE","PROFILE_TEXT_COLOR","PROFILE_BG_COLOR"))]
data.cleanC2 <- data.clean[ , -which(names(data.clean) %in% c("CREATED","IS_DEFAULT_PROFILE","IS_DEFAULT_PROFILE_IMAGE","IS_BACKGROUND_IMAGE_USED","ORIGINAL_PROFILE_IMAGE","BACKGROUND_IMAGE","PROFILE_TEXT_COLOR","PROFILE_BG_COLOR"))]

#print importance of variables
imp <- function(data) {
  cat("\n")
  print("Information Gain:")
  
  weights <- information.gain(CLASS~., data)
  print(weights)

  cat("\n")
  print("Gain ratio:")
  
  weights <- gain.ratio(CLASS~., data)
  print(weights)
  
  cat("\n")
  print("Symmetrical uncertainty:")
  
  weights <- symmetrical.uncertainty(CLASS~., data)
  print(weights)
  
  cat("\n")
  print("OneR:")
  
  weights <- oneR(CLASS~., data)
  print(weights)
  
  cat("\n")
  print("Random forest Accuracy:")
  
  weights2 <- random.forest.importance(CLASS ~ ., data, 1)
  print(weights2)

  cat("\n")
  print("Random forest Impurity:")
  
  weights2 <- random.forest.importance(CLASS ~ ., data, 2)
  print(weights2)

  cat("\n")
  print("CFS:")
  
  weights3 <- cfs(CLASS~., data)
  print(weights3)
  
  cat("\n")
  print("Chi squared:")
  
  weights3 <- chi.squared(CLASS~., data)
  print(weights3)

  cat("\n")
  print("Linear correlation:")
  
  weights4 <- linear.correlation(CLASS~., data)
  print(weights4)
}

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/AttrImportance.txt"
sink(filename, append = TRUE)

cat("\n")
print("Attribute importance - Dataset B")
print("=================================")

imp(data.cleanB)

cat("\n")
print("Attribute importance - Dataset B2")
print("=================================")

imp(data.cleanB2)

cat("\n")
print("Attribute importance - Dataset C")
print("=================================")

imp(data.cleanC)

cat("\n")
print("Attribute importance - Dataset C2")
print("=================================")

imp(data.cleanC2)


#subset <- cutoff.k(weights, 2)
#f <- as.simple.formula(subset, "CLASS")
#print(f)


library(ggplot2)
library(reshape2)
d <- melt(data.cleanC2)
# Basic box plot
p <- ggplot(d, aes(x=CLASS, y=value, fill=CLASS)) + 
  facet_wrap(~variable,scales = "free_x") +
  geom_boxplot() +
  coord_flip() +
  geom_boxplot(notch=FALSE)
  
print(p)

p <- ggplot(d, aes(x=value, fill=CLASS)) + 
  facet_wrap(~variable,scales = "free_x") +
  geom_histogram(bins=100)

print(p)

p <- ggplot(d, aes(value, fill=CLASS)) + 
  facet_wrap(~variable, scales = "free_x") +
  geom_density(alpha = 0.2)

print(p)

sink()
