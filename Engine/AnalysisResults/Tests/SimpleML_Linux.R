suppressMessages(library(RODBC))
suppressMessages(library(caret))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))

#load external libraries
#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")
source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")

#LINUX
filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/C_LNX_NP_5fold_0repeat_3tune.txt"

#### connect to DB
#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_CELEBRITY, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, CLASS from twitter.zz_full_set_win") )
data.full <- data.original

#get unique values in set
#rapply(data.original,function(x)length(unique(x)))

#' ######################################
#' ###  Cleanup and preprocessing
#' ######################################
#+ clean_preprocess
#change factors to characters
data.full <- cleanup.factors(data.full)
data.clean <- cleanup.Twitter(data.full)

#' ######################################
#' ### Prepare Datasets with dummy vars
#' ######################################
#+ prepare
data.o <- data.clean

#' ######################################
#' ### Run model and print results
#' ######################################
#+ run_models
set.seed(123)

inTrain <- createDataPartition(y = data.o$CLASS, p = .75, list = FALSE)
#str(inTrain)

training <- data.o[inTrain,]
testing <- data.o[-inTrain,]
rm(inTrain)  

folds <- 5
repeats <- 0
resamp <- "cv"
tune <- 3

t <- system.time(ML_Models_ROC(training, resamp, folds, tune, repeats, filename))

sink(filename, append = TRUE)

cat("\n")
print("Query loading run time")
print("==============")
print(tl)

cat("\n")
print("Models run time")
print("==============")
print(t)

sink()