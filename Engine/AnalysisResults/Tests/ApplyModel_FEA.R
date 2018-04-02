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

work <- function(sqlSaveTable, fn){
  #' ###Load data
  #+ get_data
  tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, FOLLOWERS_COUNT, FRIENDS_COUNT, FF_RATIO, LISTED_COUNT, USERNAME_LENGTH, GEO_ENABLED, PROFILE_HAS_URL, IS_CELEBRITY, ACCOUNT_AGE_IN_MONTHS, LANGUAGE, HAS_NAME, HAS_IMAGE, DUP_PROFILE, HAS_PROFILE, STATUS_COUNT, CLASS from TWITTER.ZZ_BOT_SET") )
  
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
  data.full <- data.full[ , -which(names(data.full) %in% c("IS_CELEBRITY"))]  #ID, SCREENNAME
  
  #change factors to characters
  data.full <- cleanup.factors(data.full)
  #clean
  data.clean <- cleanup.TwitterBot(data.full)
  
  #was done in cleanup
  #remove fields not in fake accounts
  data.clean <- data.clean[ , -which(names(data.clean) %in% c("LANGUAGE"))]
  
  data.bot <- data.clean
  
  #next set of data
  tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from TWITTER.ZZ_FE_SET") )
  
  data.full <- data.original
  
  #change factors to characters
  data.full <- cleanup.factors(data.full)
  #clean
  data.clean <- cleanup.TwitterFE(data.full)
  
  #remove fields not in fake accounts
  data.clean <- data.clean[ , -which(names(data.clean) %in% c("FF_RATIO","PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
  data.clean <- data.clean[ , -which(names(data.clean) %in% c("HAMMING"))]
  
  #perform machine learning
  data.fe <- data.clean
  
  #merge bot and fe data
  data.o <- merge(data.bot, data.fe, by.x = c("ID","SCREENNAME","CLASS"), by.y = c("ID","SCREENNAME","CLASS"))
  tmp <- data.o[,-which(names(data.o) %in% c("ID","SCREENNAME","CLASS"))]
  data.o <- cbind(tmp, CLASS=data.o$CLASS)
  
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

work("TWITTER.PREDFEA_20180402", "PREDFEA_20180402.txt")

close(myconn)