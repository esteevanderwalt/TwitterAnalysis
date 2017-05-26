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
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from TWITTER.ZZ_RFE_SET") )

close(myconn)

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

#first get nzv values
nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
nzv[nzv$nzv,]

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
#,"none","smote"

cl <- makeCluster(detectCores())
registerDoParallel(cores=6) #or cl
for (m in sampling) {
  for (x in folds) {
    for (y in repeats) {
      for (z in tune) {
        filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/RFE4_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,".txt",sep="")
        #print(filename)
        t <- system.time(ML_Models_ROC_P(training, resamp, x, z, y, m, filename, 1))
        
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
stopCluster(cl)