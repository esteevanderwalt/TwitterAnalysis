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
source("LIB_Stats.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, CLASS from twitter.zz_full_set") )
close(myconn)

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

#clean
data.clean <- cleanup.TwitterNA(data.full)

#impute - fill missing gaps
data.clean[data.clean$CLASS=='deceptive',]$LOCATION = NA

c <- c("LOCATION","CLASS")
data.clean.location <- complete(mice(as.data.frame(data.clean[c]), m=1, maxit = 1, method = 'pmm', seed = 500),1)

data <- data.clean.location
fake <- data[data$CLASS=='deceptive',"LOCATION"]
success <- data[data$CLASS!='deceptive',"LOCATION"]
x <- wilcox.test(as.data.frame(success), as.data.frame(fake))  

df <- as.data.frame(fake)
str(df)

#discretize
data.clean <- cleanup.Twitter(data.full)

