
library(RPostgreSQL)
library(caret)
library(lubridate)
library(doParallel)
library(pROC)
library(dplyr)
library(ggplot2)
library(reshape2)

#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "twitter",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "")
#rm(pw) # removes the password

data.original <- dbGetQuery(con, "SELECT * from main.zz_full_set") 
data.full <- data.original

#CHECK MISSING DATA
# A function that plots missingness

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

######################################
##  CLEANUP AND PREPROCESSING
######################################
data.full$sentiment[is.na(data.full$sentiment)] <- 'Other'
data.full$emotion[is.na(data.full$emotion)] <- 'Other'
data.full$distance_location[is.na(data.full$distance_location)] <- 0
data.full$distance_tz[is.na(data.full$distance_tz)] <- 0
data.full$continent[is.na(data.full$continent)] <- 'Other'
data.full$sub_region[is.na(data.full$sub_region)] <- 'Other'
data.full$gender[is.na(data.full$gender)] <- 'Other'
data.full$avg_tweet_time[is.na(data.full$avg_tweet_time)] <- 12
data.full$no_of_devices[is.na(data.full$no_of_devices)] <- 1
data.full$levenshtein[is.na(data.full$levenshtein)] <- 1
data.full$hamming[is.na(data.full$hamming)] <- 1
data.full$valid_name[is.na(data.full$valid_name)] <- 0
data.full$image_gender[is.na(data.full$image_gender)] <- 'Other'
data.full$image_age[is.na(data.full$image_age)] <- 20
data.full$no_of_faces[is.na(data.full$no_of_faces)] <- 20
#change last tweet time
data.full$last_tweet_time <- year(ymd_hms(data.full$last_tweet_time))
data.full$last_tweet_time[is.na(data.full$last_tweet_time)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(continent) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(continent %in% d$continent[1:50]))$continent
data.full$continent[data.full$continent %in% l] <- 'Other'
rm(d, l)
#remove decimals from numerics
data.full$image_age <- round(data.full$image_age)
data.full$avg_tweet_time <- round(data.full$avg_tweet_time)
#update name
data.full[data.full$valid_name != 0,]$valid_name <- 1
#first replace NA with other
data.full$timezone[is.na(data.full$timezone)] <- 'Other'
data.full$latitude[is.na(data.full$latitude)] <- 0
data.full$longitude[is.na(data.full$longitude)] <- 0
#change created to be year of creation
data.full$created <- year(ymd_hms(data.full$created))
data.full$created[is.na(data.full$created)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(location) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(location %in% d$location[1:50]))$location
data.full$location[data.full$location %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(timezone) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(timezone %in% d$timezone[1:20]))$timezone
data.full$timezone[data.full$timezone %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(language) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(language %in% d$language[1:20]))$language
data.full$language[data.full$language %in% l] <- 'Other'
rm(d, l)
#remove decimals from lat/lon
data.full$latitude <- round(data.full$latitude)
data.full$longitude <- round(data.full$longitude)

########################################
## BUILD SETS
########################################
prepareData <- function(x){
  
  #identify nonzero attributes that can influence result
  nzv <- nearZeroVar(x, saveMetrics= TRUE)
  print(nzv[nzv$nzv,])
  
  #check that there are no missing values
  p <- ggplot_missing(x)
  print(p)
  
  dmy <- dummyVars("class ~ .", data = x, fullRank=T)
  y <- data.frame(predict(dmy, newdata = x))
  
  #identify correlated predictors
  descrCor <-  cor(y)
  highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
  print(highCorr)
  
  #done automatically?
  #rm(dmy, nzv, descrCor, highCorr)
  y <- cbind(y,x$class)
  colnames(y)[colnames(y) == 'x$class'] <- 'class'
  return(y)
}

###############################
## PREPARE DATASETS
###############################
myvars <- c("utc_offset",
            "geo_enabled", "latitude", "longitude",  
            "is_default_profile", "is_default_profile_image", "created", "class")
data.o <- prepareData(data.full[myvars])

myvars <- c("distance_location","distance_tz",
            "gender","levenshtein","hamming","valid_name","image_gender","image_age",
            "no_of_faces", "class")
data.e <- prepareData(data.full[myvars])

####################################
##  FUNCTION for partial least squares
####################################
runModel <- function(x, s, m, saved){
  set.seed(123)
  inTrain <- createDataPartition(y = x$class, p = .75, list = FALSE)
  #str(inTrain)
  
  training <- x[inTrain,]
  testing <- x[-inTrain,]
  rm(inTrain)  
  
  ctrl <- trainControl(method = "repeatedcv", 
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
  
  #cl <- makeCluster(detectCores())
  #registerDoParallel(cl)
  s <- paste(s,m,"Fit.RData", sep="_")
  if(saved == 0){
    plsFit <- train(class ~ ., data = training,
                    method = m, 
                    tuneLength = 15, 
                    trControl = ctrl, 
                    metric = "ROC",
                    preProc = c("center", "scale"))
    save(plsFit,file=s)    
  }else{
    load(s)
    #stopCluster(cl)
    #registerDoSEQ()
  }
  
  #show result
  print(plsFit)
  p <- plot(plsFit)
  print(p)
  #plot(plsFit, metric="Kappa")
  #ggplot(plsFit)
  
  plsImportance <- varImp(plsFit, scale=FALSE)
  p <- plot(plsImportance)
  print(p)
  
  #predict new values
  plsClasses <- predict(plsFit, newdata = testing)
  print(head(plsClasses))
  
  plsProbs <- predict(plsFit, newdata = testing, type = "prob")
  print(head(plsProbs))
  
  p <- confusionMatrix(data = plsClasses, testing$class)
  print(p)
  
  #ROC
  plsRoc <- roc(testing$class,plsProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
  print(plsRoc)
  p <- plot(plsRoc, print.thres="best", print.thres.best.method="closest.topleft")
  print(p)
  plsRocCoords <- coords(  plsRoc, "best", best.method="closest.topleft",
                           ret=c("threshold", "accuracy"))
  print(plsRocCoords)
  
  rm(plsRoc, plsRocCoords, plsProbs, plsClasses, plsImportance)
  
  return(plsFit)
}

resampling <- function(x){
  #How do these models compare in terms of their resampling results? The resamples function can be
  #used to collect, summarize and contrast the resampling results. Since the random number seeds
  #were initialized to the same value prior to calling train, the same folds were used for each model.
  resamps <- resamples(x)
  summary(resamps)
  p <- xyplot(resamps, what = "BlandAltman") 
  print(p)
  # boxplots of results
  p <- bwplot(resamps)
  print(p)
  # dot plots of results
  p <- dotplot(resamps)
  print(p)
  
  #Since, for each resample, there are paired results a paired t–test can be used to assess whether there
  #is a difference in the average resampled area under the ROC curve. The diff.resamples function
  #can be used to compute this:
  diffs <- diff(resamps)
  print(summary(diffs))
  
}

#data = full set with class
#dummy = dummyVars set without class
plsFit.o <- runModel(data.o, "original","pls", 0)
plsFit.e <- runModel(data.e, "engineer","pls", 0)
resampling(list(opls = plsFit.o, epls = plsFit.e))

adaFit.o <- runModel(data.o, "original","adaboost", 0)
adaFit.e <- runModel(data.e, "engineer","adaboost", 0)
resampling(list(oada = adaFit.o, eada = adaFit.e))

xyfFit.o <- runModel(data.o, "original","xyf", 0)
xyfFit.e <- runModel(data.e, "engineer","xyf", 0)
resampling(list(oxyf = xyfFit.o, exyf = xyfFit.e))

svmFit.o <- runModel(data.o, "original","svmRadial", 0)
svmFit.e <- runModel(data.e, "engineer","svmRadial", 0)
resampling(list(osvm = svmFit.o, esvm = svmFit.e))

