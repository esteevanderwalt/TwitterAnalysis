suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(RODBC))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(doParallel))
suppressMessages(library(FSelector))
suppressMessages(library(pROC)) # for AUC calculations
suppressMessages(library(PRROC)) # for Precision-Recall curve calculations
suppressMessages(library(MLmetrics)) # for prSummary in Caret

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")
source("LIB_Stats.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, FF_RATIO, USERNAME_LENGTH, PROFILE_HAS_URL, ACCOUNT_AGE_IN_MONTHS, HAS_NAME, HAS_IMAGE, DUP_PROFILE, HAS_PROFILE, CLASS from twitter.zz_together_set") )

close(myconn)

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

data.full <- data.full[ , -which(names(data.full) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.Twitter(data.full)
data.clean <- cleanup.TwitterBot(data.clean)

#remove NZV values
#was done in cleanup
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]

#remove fields not in fake accounts
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("BACKGROUND_IMAGE", "IS_BACKGROUND_IMAGE_USED", "PROFILE_TEXT_COLOR", "PROFILE_BG_COLOR"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("CREATED", "ORIGINAL_PROFILE_IMAGE"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("GEO_ENABLED", "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE"))]

#remove unique values
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("LOCATION", "LONGITUDE", "LATITUDE"))]

#remove language and timezone
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("LANGUAGE", "TIMEZONE", "PROFILE_IMAGE"))]

#scaled down version
data.clean <- data.clean[ , -which(names(data.clean) %in% c("ID", "NAME", "SCREENNAME", "CREATED", "ORIGINAL_PROFILE_IMAGE", "BACKGROUND_IMAGE", "LAST_TWEET", "DESCRIPTION", "LOCATION", "FOLLOWERS_COUNT", "STATUS_COUNT", "LISTED_COUNT", "TIMEZONE", "LATITUDE", "LONGITUDE", "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE", "IS_BACKGROUND_IMAGE_USED", "PROFILE_TEXT_COLOR", "PROFILE_BG_COLOR", "FF_RATIO", "USERNAME_LENGTH", "PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "HAS_NAME", "HAS_IMAGE", "HAS_PROFILE"))]

#perform machine learning
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


################################
## Run various params on dataset
################################
resamp <- "repeatedcv"
#folds
folds <- c(10)
#repeats
repeats <- c(3)
#tune
tune <- c(3)
#sampling
sampling <- c("smote")  
#summary function
summF <- c("twoClassSummary", "prSummary")

cl <- makeCluster(detectCores())
registerDoParallel(cores=7) #or cl
for (n in summF) {
  for (m in sampling) {
    for (x in folds) {
      for (y in repeats) {
        for (z in tune) {
          filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/TO2_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,".txt",sep="")
          imagefilename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/TO2_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,"_",sep="")
          
          #print(filename)
          t <- system.time(ML_Models_ROC_P(training, resamp, x, z, y, m, filename, imagefilename, 0, n))        
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
        }
      }
    }
  }
}
stopCluster(cl)