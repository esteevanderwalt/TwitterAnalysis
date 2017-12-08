#use Racing to select the best technique for an unbalanced dataset
library(unbalanced)
data(ubIonosphere)

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

#perform hopkins stat test for cluster tendency
data.o <- data.clean

data.o <- data.o[ , -which(names(data.o) %in% c("LOCATION", "LONGITUDE", "LATITUDE"))]
data.o <- data.o[ , -which(names(data.o) %in% c("LANGUAGE","UTC_OFFSET", "PROFILE_IMAGE"))]
#data.o <- data.o[ , -which(names(data.o) %in% c("CLASS"))]

#inTrain <- createDataPartition(y = data.o$CLASS, p = .99, list = FALSE)

#training <- data.o[inTrain,]
#testing <- data.o[-inTrain,]
#rm(inTrain) 

#cl <- makeCluster(detectCores())
#registerDoParallel(cores=7) #or cl
#head(iris)
df = scale(data.o[,-6])
df <- as.data.frame(cbind(df,CLASS=data.o$CLASS))
df$CLASS <- as.factor(df$CLASS)

#n<-ncol(df)
#output<-df$CLASS
#input<-df[ ,-n]
data<-ubSMOTE(X=df[ ,-6], Y=df$CLASS)
#newData<-cbind(data$X, data$Y)

#configure sampling parameters
ubConf <- list(type="ubUnder", percOver=200, percUnder=200, k=2, perc=50, method="percPos", w=NULL)

#load the classification algorithm that you intend to use inside the Race
#see 'mlr' package for supported algorithms
library(randomForest)
#use only 5 trees
results <- ubRacing(CLASS ~ ., df, "randomForest", positive=1, ubConf=ubConf, ntree=5)
r <- results$Race

# try with 500 trees
# results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=500)
# let's try with a different algorithm
# library(e1071)
# results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
# library(rpart)
# results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)