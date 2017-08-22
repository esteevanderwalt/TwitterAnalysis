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

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from TWITTER.ZZ_FE_SET") )

close(myconn)

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

getRandomData <- function(d){
  #split d in deceptive and not
  deceptive <- d[d$CLASS=="deceptive",]
  trustworthy <- d[d$CLASS=="trustworthy",]
  #randomly pick 20000 trustworthy
  trust <- trustworthy[sample(nrow(trustworthy), 60000), ]
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
#,"none","smote"
sqlSaveTable <- "NONE"
rr <- c(1,2,3,4,5,6,7,8,9,10)  

cl <- makeCluster(detectCores())
registerDoParallel(cores=7) #or cl
for (m in sampling) {
  for (x in folds) {
    for (y in repeats) {
      for (z in tune) {
        for (w in rr) {
          #get random sample, but always use same 1000 deceptive accounts
          data.r <- getRandomData(data.o)
          inTrain <- createDataPartition(y = data.r$CLASS, p = .75, list = FALSE)
          training <- data.r[inTrain,]
          testing <- data.r[-inTrain,]
          rm(inTrain)  
          
          filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/FE_SAMPLES60K_",x,"fold_",y,"repeat_",z,"tune_",m,"round_",w,".txt",sep="")
          #print(filename)
          t <- system.time(ML_Models_ROC_P_knn(training, resamp, x, z, y, m, filename, 1))
          #t <- system.time(ML_Models_apply(filename, sqlSaveTable))
          
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