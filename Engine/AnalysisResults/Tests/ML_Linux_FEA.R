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
suppressMessages(library(DMwR))

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")
source("LIB_Stats.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, FOLLOWERS_COUNT, FRIENDS_COUNT, FF_RATIO, LISTED_COUNT, USERNAME_LENGTH, GEO_ENABLED, PROFILE_HAS_URL, IS_CELEBRITY, ACCOUNT_AGE_IN_MONTHS, LANGUAGE, HAS_NAME, HAS_IMAGE, DUP_PROFILE, HAS_PROFILE, STATUS_COUNT, CLASS from TWITTER.ZZ_BOT_SET") )

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
data.full <- data.full[ , -which(names(data.full) %in% c("IS_CELEBRITY"))]  #ID, SCREENNAME

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterBot(data.full)

#was done in cleanup
#remove fields not in fake accounts
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("USERNAME_LENGTH", "GEO_ENABLED"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "DUP_PROFILE", "HAS_PROFILE"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("LANGUAGE"))]

#data.clean[data.clean$FRIENDS_COUNT > 1000,]$FRIENDS_COUNT <- 1000
#data.clean[data.clean$FOLLOWERS_COUNT > 1000,]$FOLLOWERS_COUNT <- 1000
#data.clean[data.clean$STATUS_COUNT > 1000,]$STATUS_COUNT <- 1000
#data.clean[data.clean$LISTED_COUNT > 100,]$LISTED_COUNT <- 100

#perform machine learning
data.bot <- data.clean

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
nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
nzv[nzv$nzv,]

#rapply(data.full,function(x)length(unique(x)))
#rapply(d,function(x)sum(is.na(x)))

#remove NZV values
#data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterFE(data.full)

#was done in cleanup
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("FF_RATIO","PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("USERNAME_LENGTH", "GEO_ENABLED"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "DUP_PROFILE", "HAS_PROFILE"))]

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
sampling <- c("none")
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
            for (r in 1:1) {
              filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/FEA_savemodel_smote20_15K_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,"_size_",s,"_round_",r,".txt",sep="")
              imagefilename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/FEA_savemodel_smote20_15K_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,"_size_",s,"_round_",r,"_",sep="")
              
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
              if(s > 0){
                #get new data from random sample generator
                data.r <- getRandomData(data.o, s)
                inTrain <- createDataPartition(y = data.r$CLASS, p = .75, list = FALSE)
                training <- data.r[inTrain,]
                testing <- data.r[-inTrain,]
                rm(inTrain) 
              }
              
              training <- SMOTE(CLASS ~., training, perc.over=100, perc.under=200)
              #gives 45K records - reduce that to 4.5K
              inTrain <- createDataPartition(y = training$CLASS, p = .20, list = FALSE)
              training <- training[inTrain,]
              rm(inTrain)
              print(paste("Smote completed:",nrow(training),sep=""))
              t <- system.time(ML_Models_ROC_P(training, resamp, x, z, y, m, filename, imagefilename, 1, n))        
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