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

options(scipen=999)

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

work <- function(tableName, sqlSaveTable){
  #' ###Load data
  #+ get_data
  tl <- system.time(data.original <- sqlQuery(myconn, paste("SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, CLASS FROM ", tableName, sep="") ) )
  
  #dim(data.original)
  #save(data.original,file="data.original.RData")
  #load("data.original.RData")
  data.full <- data.original
  
  #rapply(data.full,function(x)length(unique(x)))
  #rapply(d,function(x)sum(is.na(x)))
  
  #remove NZV values
  data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME","CLASS"))]
  
  #change factors to characters
  #data.full <- cleanup.factors(data.full)
  #clean
  #data.clean <- cleanup.TwitterFE(data.full)
  
  #perform machine learning
  data.scaled <- abs(scale(data.full))
  data.scaled[is.na(data.scaled)] <- 0
  
  #DISTANCE_LOCATION  0.96
  #DISTANCE_TZ 100
  #COMPARE_GENDER 27.83
  #LEVENSTHEIN  58.26
  #HAMMING  27.14
  #COMPARE_AGE  0
  s <- data.scaled[,1]*0.96 + data.scaled[,2]*100 + data.scaled[,3]*27.83 + data.scaled[,4]*58.26 + data.scaled[,5]*27.14 + data.scaled[,6]*0
  #add userid, screenname to probability results
  b <- cbind(data.original[,1:2], data.scaled, CLASS=data.original[,9], SCORE=s)
  #write back results to table  
  sqlSave(channel=myconn, b, tablename=sqlSaveTable, append=TRUE, rownames=FALSE)  
  
}

work("TWITTER.ZZ_RFE_SET_20170106_B", "TWITTER.ZZ_SCORE_20170106")
work("TWITTER.ZZ_RFE_SET_20170418_B", "TWITTER.ZZ_SCORE_20170418")
work("TWITTER.ZZ_RFE_SET_20170429_B", "TWITTER.ZZ_SCORE_20170429")
work("TWITTER.ZZ_RFE_SET_20170517_B", "TWITTER.ZZ_SCORE_20170517")
work("TWITTER.ZZ_RFE_SET_20170527_B", "TWITTER.ZZ_SCORE_20170527")


close(myconn)
