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
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, CLASS from twitter.zz_full_set") )

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

#remove NZV values
#was done in cleanup
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("BACKGROUND_IMAGE", "IS_BACKGROUND_IMAGE_USED", "PROFILE_TEXT_COLOR", "PROFILE_BG_COLOR"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("CREATED", "ORIGINAL_PROFILE_IMAGE"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("GEO_ENABLED", "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE"))]

#remove unique values
data.clean <- data.clean[ , -which(names(data.clean) %in% c("LOCATION", "LONGITUDE", "LATITUDE"))]

#remove language and timezone
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("LANGUAGE", "TIMEZONE", "PROFILE_IMAGE"))]

#perform machine learning
data.o <- data.clean

#' ######################################
#' ### Run model and print results
#' ######################################
#+ run_models
#set.seed(123)

#inTrain <- createDataPartition(y = data.o$CLASS, p = .75, list = FALSE)

#training <- data.o[inTrain,]
#testing <- data.o[-inTrain,]
#rm(inTrain)  

getRandomData <- function(d, si){
  #split d in deceptive and not
  deceptive <- d[d$CLASS=="deceptive",]
  trustworthy <- d[d$CLASS=="trustworthy",]
  #randomly pick si (size) trustworthy
  trust <- trustworthy[sample(nrow(trustworthy), si), ]
  #add to deceptive
  total <- rbind(deceptive, trust)
  #return
  return(total)
}

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
summF <- c("prSummary") #"twoClassSummary", 
rr <- c(1)  
sz <- c(0) #use 0 for full set - , 1000, 10000, 100000

cl <- makeCluster(detectCores())
registerDoParallel(cores=7) #or cl
for (n in summF) {
  for (m in sampling) {
    for (x in folds) {
      for (y in repeats) {
        for (z in tune) {
          for (s in sz) {
            for (r in 1:30) {  #rr
              filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/META6_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,"_size_",s,"_round_",r,".txt",sep="")
              imagefilename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/META6_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,"_size_",s,"_round_",r,"_",sep="")
              
              set.seed(Sys.time())
              inTrain <- createDataPartition(y = data.o$CLASS, p = .75, list = FALSE)
              #inTrain <- sample(seq_along(data.o$CLASS), as.integer(0.75 * nrow(data.o)))
              
              sink(filename)
              
              cat("\n")
              print("START INTRAIN")
              print("==============")
              print(head(inTrain))
              
              sink()
              
              training <- data.o[inTrain,]
              testing <- data.o[-inTrain,]
              
              #determine data for this test
              #if(s > 0){
              #  #get new data from random sample generator
              #  data.r <- getRandomData(data.o, s)
              #  inTrain <- createDataPartition(y = data.r$CLASS, p = .75, list = FALSE)
              #  training <- data.r[inTrain,]
              #  testing <- data.r[-inTrain,]
              #  rm(inTrain) 
              #}
              
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
  }
}
stopCluster(cl)

#run with super sampling to cater for data skewness