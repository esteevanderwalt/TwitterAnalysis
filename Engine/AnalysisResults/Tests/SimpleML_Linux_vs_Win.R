suppressMessages(library(RODBC))
suppressMessages(library(caret))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))

#### connect to DB
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
data.original <- sqlQuery(myconn, "SELECT * from twitter.zz_full_set_win")
data.full <- data.original

#get unique values in set
rapply(data.original,function(x)length(unique(x)))

#' ######################################
#' ###  Cleanup and preprocessing
#' ######################################
#+ clean_preprocess
#change factors to characters
data.full[,c("SENTIMENT","EMOTION","LANGUAGE","ORIGINAL_PROFILE_IMAGE","PROFILE_IMAGE","BACKGROUND_IMAGE","TIMEZONE",
             "PROFILE_TEXT_COLOR","PROFILE_BG_COLOR","CLASS")] <- sapply(data.full[,c("SENTIMENT","EMOTION","LANGUAGE","ORIGINAL_PROFILE_IMAGE","PROFILE_IMAGE","BACKGROUND_IMAGE","TIMEZONE",
                                                                                 "PROFILE_TEXT_COLOR","PROFILE_BG_COLOR","CLASS")],as.character) 

data.full$SENTIMENT[is.na(data.full$SENTIMENT)] <- 'Other'
data.full$EMOTION[is.na(data.full$EMOTION)] <- 'Other'
data.full$DISTANCE_LOCATION[is.na(data.full$DISTANCE_LOCATION)] <- 0
data.full$DISTANCE_TZ[is.na(data.full$DISTANCE_TZ)] <- 0
data.full$UTC_OFFSET[is.na(data.full$UTC_OFFSET)] <- 0
data.full$IS_DEFAULT_PROFILE_IMAGE[is.na(data.full$IS_DEFAULT_PROFILE_IMAGE)] <- 0
data.full$CONTINENT[is.na(data.full$CONTINENT)] <- 'Other'
data.full$SUB_REGION[is.na(data.full$SUB_REGION)] <- 'Other'
data.full$GENDER[is.na(data.full$GENDER)] <- 'Other'
data.full$GENDER[data.full$GENDER=='1M'] <- 'M'
data.full$GENDER[data.full$GENDER=='?M'] <- 'M'
data.full$GENDER[data.full$GENDER=='1F'] <- 'F'
data.full$GENDER[data.full$GENDER=='?F'] <- 'F'
data.full$AVG_TWEET_TIME[is.na(data.full$AVG_TWEET_TIME)] <- 12
data.full$NO_OF_DEVICES[is.na(data.full$NO_OF_DEVICES)] <- 1
data.full$LEVENSHTEIN[is.na(data.full$LEVENSHTEIN)] <- 1
data.full$HAMMING[is.na(data.full$HAMMING)] <- 1
data.full$VALID_NAME[is.na(data.full$VALID_NAME)] <- 0
data.full$IMAGE_GENDER[is.na(data.full$IMAGE_GENDER)] <- 'Other'
data.full$IMAGE_AGE[is.na(data.full$IMAGE_AGE)] <- 20
data.full$NO_OF_FACES[is.na(data.full$NO_OF_FACES)] <- 20
#change last tweet time
data.full$LAST_TWEET_TIME <- year(ymd_hms(data.full$LAST_TWEET_TIME))
data.full$LAST_TWEET_TIME[is.na(data.full$LAST_TWEET_TIME)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(CONTINENT) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(CONTINENT %in% d$CONTINENT[1:50]))$CONTINENT
data.full$CONTINENT[data.full$CONTINENT %in% l] <- 'Other'
rm(d, l)
#remove decimals from numerics
data.full$IMAGE_AGE <- round(data.full$IMAGE_AGE)
data.full$AVG_TWEET_TIME <- round(data.full$AVG_TWEET_TIME)
#update name
data.full[data.full$VALID_NAME != 0,]$VALID_NAME <- 1
#first replace NA with other
data.full$TIMEZONE[is.na(data.full$TIMEZONE)] <- 'Other'
data.full$LATITUDE[is.na(data.full$LATITUDE)] <- 0
data.full$LONGITUDE[is.na(data.full$LONGITUDE)] <- 0
#change created to be year of creation
data.full$CREATED <- year(ymd_hms(data.full$CREATED))
data.full$CREATED[is.na(data.full$CREATED)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(LOCATION) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(LOCATION %in% d$LOCATION[1:50]))$LOCATION
data.full$LOCATION[data.full$LOCATION %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(TIMEZONE) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(TIMEZONE %in% d$TIMEZONE[1:20]))$TIMEZONE
data.full$TIMEZONE[data.full$TIMEZONE %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(LANGUAGE) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(LANGUAGE %in% d$LANGUAGE[1:20]))$LANGUAGE
data.full$LANGUAGE[data.full$LANGUAGE %in% l] <- 'Other'
rm(d, l)
#remove decimals from lat/lon
data.full$LATITUDE <- round(data.full$LATITUDE)
data.full$LONGITUDE <- round(data.full$LONGITUDE)

#' ######################################
#' ### Prepare Datasets with dummy vars
#' ######################################
#+ prepare
myvars <- c("UTC_OFFSET",
            "GEO_ENABLED", "LATITUDE", "LONGITUDE",  
            "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE", "CLASS")

prepareData <- function(x){
  
  #identify nonzero attributes that can influence result
  nzv <- nearZeroVar(x, saveMetrics= TRUE)
  print(nzv[nzv$nzv,])
  
  dmy <- dummyVars("CLASS ~ .", data = x, fullRank=T)
  y <- data.frame(predict(dmy, newdata = x))
  
  #identify correlated predictors
  descrCor <-  cor(y)
  highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
  print(highCorr)
  
  #done automatically?
  #rm(dmy, nzv, descrCor, highCorr)
  y <- cbind(y,x$CLASS)
  colnames(y)[colnames(y) == 'x$CLASS'] <- 'CLASS'
  return(y)
}

data.o <- prepareData(data.full[myvars])

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

#--------------------------------------
# Model 1 - SVM Radial
#--------------------------------------
# Model notes:
#   warnings() == F
#   sigma == constant (0.547204984213807)
#   C == varied, 3 times (0.25,0.5,1.0)

# Specify fit parameters
fit.m1.fc <- trainControl(method = resamp,
                          number = folds,
                          repeats = repeats,
                          classProbs = T,
                          summaryFunction = twoClassSummary)

# Build model
set.seed(123)
#find missing values
sapply(training, function(x) sum(is.na(x)))

m1.t <- system.time(fit.m1 <- train(CLASS~., data=training,
                                    method = "svmRadial",
                                    metric = "ROC",
                                    preProcess = c("center", "scale"),
                                    trControl = fit.m1.fc,
                                    tuneLength = tune))

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/test.txt"

sink(filename, append = TRUE)

# In-sample summary
fit.m1$finalModel
fit.m1$results

cat("\n")
print("Total run time")
print("==============")
print(m1.t)

sink()



