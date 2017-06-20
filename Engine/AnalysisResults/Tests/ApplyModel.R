suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(RODBC))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(doParallel))
suppressMessages(library(FSelector))
suppressMessages(library(pROC))

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")
source("LIB_Stats.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

work <- function(tableName, sqlSaveTable, fn){
  #' ###Load data
  #+ get_data
  tl <- system.time(data.original <- sqlQuery(myconn, paste("SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from ", tableName, sep="") ) )
  
  #dim(data.original)
  #save(data.original,file="data.original.RData")
  #load("data.original.RData")
  data.full <- data.original
  
  #first get nzv values
  #nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
  #nzv[nzv$nzv,]
  
  #rapply(data.full,function(x)length(unique(x)))
  #rapply(d,function(x)sum(is.na(x)))
  
  #remove NZV values
  data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME"))]
  
  #change factors to characters
  data.full <- cleanup.factors(data.full)
  #clean
  data.clean <- cleanup.TwitterFE(data.full)
  
  #was done in cleanup
  #remove fields not in fake accounts
  data.clean <- data.clean[ , -which(names(data.clean) %in% c("FF_RATIO","PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
  #data.clean <- data.clean[ , -which(names(data.clean) %in% c("USERNAME_LENGTH", "GEO_ENABLED"))]
  #data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "DUP_PROFILE", "HAS_PROFILE"))]
  
  #perform machine learning
  data.o <- data.clean
  
  #' ######################################
  #' ### Run model and print results
  #' ######################################
  #+ run_models
  set.seed(123)
  
  testing <- data.o
  
  ################################
  ## Run various params on dataset
  ################################
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cores=8) #or cl
  filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/",fn,sep="")
  
  t <- system.time(ML_Models_apply(filename, sqlSaveTable, data.original, testing))
  
  stopCluster(cl)
}

#work("TWITTER.ZZ_RFE_SET_20170418_B", "TWITTER.PRED_20170418_B", "PRED_20170418_B.txt")
#work("TWITTER.ZZ_RFE_SET_20170429_B", "TWITTER.PRED_20170429_B", "PRED_20170429_B.txt")
#work("TWITTER.ZZ_RFE_SET_20170517_B", "TWITTER.PRED_20170517_B", "PRED_20170517_B.txt")
#work("TWITTER.ZZ_RFE_SET_20170527_B", "TWITTER.PRED_20170527_B", "PRED_20170527_B.txt")

#work("TWITTER.ZZ_RFE_SET_20170418", "TWITTER.PREDR_20170418", "PREDR_20170418.txt")
#work("TWITTER.ZZ_RFE_SET_20170429", "TWITTER.PREDR_20170429", "PREDR_20170429.txt")
#work("TWITTER.ZZ_RFE_SET_20170517", "TWITTER.PREDR_20170517", "PREDR_20170517.txt")
#work("TWITTER.ZZ_RFE_SET_20170527", "TWITTER.PREDR_20170527", "PREDR_20170527.txt")
#work("TWITTER.ZZ_RFE_SET_20170418_B", "TWITTER.PREDR_20170418_B", "PREDR_20170418_B.txt")
#work("TWITTER.ZZ_RFE_SET_20170429_B", "TWITTER.PREDR_20170429_B", "PREDR_20170429_B.txt")
#work("TWITTER.ZZ_RFE_SET_20170517_B", "TWITTER.PREDR_20170517_B", "PREDR_20170517_B.txt")
#work("TWITTER.ZZ_RFE_SET_20170527_B", "TWITTER.PREDR_20170527_B", "PREDR_20170527_B.txt")

work("TWITTER.ZZ_FE_FAKE_SET", "TWITTER.PREDF", "PREDF.txt")

close(myconn)