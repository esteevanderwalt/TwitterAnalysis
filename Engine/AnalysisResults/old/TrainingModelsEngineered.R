
library(RPostgreSQL)
library(caret)
library(lubridate)
library(doParallel)
library(pROC)
library(dplyr)
library(ggplot2)

#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "twitter",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "")
#rm(pw) # removes the password

users.all <- dbGetQuery(con, "SELECT * from main.zz_full_set") 
myvars <- c("sentiment","emotion","distance_location","distance_tz","continent",
            "sub_region","gender","avg_tweet_time","no_of_devices","levenshtein",
            "hamming","valid_name","image_gender","image_age","no_of_faces",
            "last_tweet_time", "class")
users1.data <- users.all[myvars]
rm(myvars)

#CHECK MISSING DATA
# A function that plots missingness
# requires `reshape2`
library(reshape2)
library(ggplot2)

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
ggplot_missing(users.all)

#PREPROCESSING OF DATA
users1.data$sentiment[is.na(users1.data$sentiment)] <- 'Other'
users1.data$emotion[is.na(users1.data$emotion)] <- 'Other'
users1.data$distance_location[is.na(users1.data$distance_location)] <- 0
users1.data$distance_tz[is.na(users1.data$distance_tz)] <- 0
users1.data$continent[is.na(users1.data$continent)] <- 'Other'
users1.data$sub_region[is.na(users1.data$sub_region)] <- 'Other'
users1.data$gender[is.na(users1.data$gender)] <- 'Other'
users1.data$avg_tweet_time[is.na(users1.data$avg_tweet_time)] <- 12
users1.data$no_of_devices[is.na(users1.data$no_of_devices)] <- 1
users1.data$levenshtein[is.na(users1.data$levenshtein)] <- 1
users1.data$hamming[is.na(users1.data$hamming)] <- 1
users1.data$valid_name[is.na(users1.data$valid_name)] <- 0
users1.data$image_gender[is.na(users1.data$image_gender)] <- 'Other'
users1.data$image_age[is.na(users1.data$image_age)] <- 20
users1.data$no_of_faces[is.na(users1.data$image_age)] <- 20

#change last tweet time
users1.data$last_tweet_time <- year(ymd_hms(users1.data$last_tweet_time))
users1.data$last_tweet_time[is.na(users1.data$last_tweet_time)] <- 2000

#change location, language, timezone to only have top50 and other
d <- users1.data %>% 
  group_by(continent) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(users1.data, !(continent %in% d$continent[1:50]))$continent
users1.data$continent[users1.data$continent %in% l] <- 'Other'
rm(d, l)

#remove decimals from numerics
users1.data$image_age <- round(users1.data$image_age)
users1.data$avg_tweet_time <- round(users1.data$avg_tweet_time)

users1.data[users1.data$valid_name != 0,]$valid_name <- 1

#select only those attributes worthy of ML
myvars <- c("distance_location","distance_tz","continent",
            "sub_region","gender","avg_tweet_time","no_of_devices","levenshtein",
            "hamming","valid_name","image_gender","image_age","no_of_faces",
            "last_tweet_time", "class")
ml1.full <- users1.data[myvars]
rm(myvars)
#identify nonzero attributes that can influence result
nzv <- nearZeroVar(ml1.full, saveMetrics= TRUE)
nzv[nzv$nzv,]
rm(nzv)

#create dummy vars
dmy <- dummyVars("class ~ .", data = ml1.full, fullRank=T)
ml1 <- data.frame(predict(dmy, newdata = ml1.full))
rm(dmy)

#identify correlated predictors
descrCor <-  cor(ml1)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
rm(descrCor, highCorr)

#start training of data
set.seed(123)
inTrain <- createDataPartition(y = ml1.full$class, p = .75, list = FALSE)
#str(inTrain)

training <- ml1[ inTrain,]
testing <- ml1[-inTrain,]
training.class <- ml1.full[ inTrain,]$class
testing.class <- ml1.full[ -inTrain,]$class
#nrow(training)
#nrow(testing)
rm(inTrain)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
plsFit <- train(x = training, y = training.class,
                method = "pls", 
                tuneLength = 15, 
                trControl = ctrl, 
                metric = "ROC",
                preProc = c("center", "scale"))
save(plsFit,file="plsFit.RData")

#load("plsFit.RData")
#stopCluster(cl)
#registerDoSEQ()

#show result
plsFit
plot(plsFit)
#plot(plsFit, metric="Kappa")
ggplot(plsFit)

plsImportance <- varImp(plsFit, scale=FALSE)
plot(plsImportance)

#predict new values
plsClasses <- predict(plsFit, newdata = testing)
head(plsClasses)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

confusionMatrix(data = plsClasses, testing.class)

#ROC
#you could look at all predictions over all partitions and resamples at once:
#head(plsFit$pred)
#plot(roc(predictor = plsFit$pred$class, response = plsFit$pred$obs))
#or for each sample
#library(plyr)
#l_ply(split(plsFit$pred, plsFit$pred$Resample), function(d) {
#  plot(roc(predictor = d$class, response = d$obs))
#})

#library(ggplot2)
#library(plotROC)
#selectedIndices <- rfFit$pred$mtry == 2
#ggplot(plsFit$pred[selectedIndices, ], 
#       aes(m = class, d = factor(obs, levels = c("trustworthy","deceptive")))) + 
#  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()
plsRoc <- roc(testing.class,plsProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
plsRoc
plot(plsRoc, print.thres="best", print.thres.best.method="closest.topleft")
plsRocCoords <- coords(  plsRoc, "best", best.method="closest.topleft",
                         ret=c("threshold", "accuracy"))
plsRocCoords

rm(plsRoc, plsRocCoords, plsProbs, plsClasses, plsImportance)

#For example, to fit a regularized discriminant model to these data, the following syntax can be used:
## To illustrate, a custom grid is used
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)

rdaFit <- train(x = training, y = training.class,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")
save(rdaFit,file="rdaFit.RData")

#load("rdaFit.RData")
rdaFit
plot(rdaFit)

rdaImportance <- varImp(rdaFit, scale=FALSE)
plot(rdaImportance)

rdaClasses <- predict(rdaFit, newdata = testing)
rdaProbs <- predict(rdaFit, newdata = testing, type = "prob")
confusionMatrix(rdaClasses, testing.class)

rdaRoc <- roc(testing.class,rdaProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
rdaRoc
plot(rdaRoc, print.thres="best", print.thres.best.method="closest.topleft")
rdaRocCoords <- coords(  rdaRoc, "best", best.method="closest.topleft",
                         ret=c("threshold", "accuracy"))
rdaRocCoords

rm(rdaRoc, rdaRocCoords, rdaProbs, rdaClasses, rdaImportance)

##############################
#Linear SVM
##############################
# creation of weights - also fast for very big datasets
#weights <- as.numeric(y[-indexes_y_test])
#for(val in unique(weights)) {weights[weights==val]=1/sum(weights==val)*length(weights)/2} # normalized to sum to length(samples)
set.seed(123)

svmFit <- train(x = training, y = training.class,
                method = "svmLinear",
                #weights = weights,
                maximize = T,
                #tuneGrid = expand.grid(.C=3^(-15:15)),
                trControl = ctrl,
                metric = "ROC")
save(svmFit,file="svmFit.RData")

#load("svmFit.RData")
svmFit
plot(svmFit)

svmImportance <- varImp(svmFit, scale=FALSE)
plot(svmImportance)

svmClasses <- predict(svmFit, newdata = testing)
svmProbs <- predict(svmFit, newdata = testing, type = "prob")
svmProbs
confusionMatrix(svmClasses, testing.class)

svmRoc <- roc(testing.class,svmProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
svmRoc
plot(svmRoc, print.thres="best", print.thres.best.method="closest.topleft")
svmRocCoords <- coords(  svmRoc, "best", best.method="closest.topleft",
                         ret=c("threshold", "accuracy"))
svmRocCoords

rm(svmRoc, svmRocCoords, svmProbs, svmClasses, svmImportance)

##############################
#Random forest
##############################
set.seed(123)
rfFit <- train(x = training, y = training.class,
               method = "rf",
               trControl = ctrl,
               metric = "ROC",
               preProcess = c("center","scale"), 
               tuneLength = 15)
save(rfFit,file="rfFit.RData")

#load("rfFit.RData")
rfFit
plot(rfFit)

# list the chosen features
predictors(rfFit)
# plot the results
plot(rfFit, type=c("g", "o"))

rfImportance <- varImp(rfFit, scale=FALSE)
plot(rfImportance)

rfClasses <- predict(rfFit, newdata = testing)
rfProbs <- predict(rfFit, newdata = testing, type = "prob")
rfProbs
confusionMatrix(rfClasses, testing.class)

rfRoc <- roc(testing.class,rfProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
rfRoc
plot(rfRoc, print.thres="best", print.thres.best.method="closest.topleft")
rfRocCoords <- coords(  rfRoc, "best", best.method="closest.topleft",
                        ret=c("threshold", "accuracy"))
rfRocCoords

rm(rfRoc, rfRocCoords, rfProbs, rfClasses, rfImportance)

##############################
#KNN
##############################
set.seed(123)
knnFit <- train(x = training, y = training.class,
                method = "knn",
                trControl = ctrl,
                metric = "ROC",
                preProcess = c("center","scale"), 
                tuneLength = 20)
save(knnFit,file="knnFit.RData")

#load("knnFit.RData")
knnFit
plot(knnFit)

knnImportance <- varImp(knnFit, scale=FALSE)
plot(knnImportance)

knnClasses <- predict(knnFit, newdata = testing)
knnProbs <- predict(knnFit, newdata = testing, type = "prob")
knnProbs
confusionMatrix(knnClasses, testing.class)

knnRoc <- roc(testing.class,knnProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
knnRoc
plot(knnRoc, print.thres="best", print.thres.best.method="closest.topleft")
knnRocCoords <- coords(  knnRoc, "best", best.method="closest.topleft",
                         ret=c("threshold", "accuracy"))
knnRocCoords

rm(knnRoc, knnRocCoords, knnProbs, knnClasses, knnImportance)

##############################
# C5.0 - Boosting
##############################
set.seed(123)
c50fit <- train(x=training, y=ctraining, 
                method = "C5.0", 
                metric = "ROC", 
                trControl=control,
                verbose=FALSE)
save(c50fit,file="c50fit.RData")

#load("c50Fit.RData")
c50fit
# visualize the resample distributions
xyplot(c50fit,type = c("g", "p", "smooth"))

plot(c50fit)

c50Importance <- varImp(c50fit, scale=FALSE)
plot(c50Importance)

c50Classes <- predict(c50fit, newdata = testing)
c50Probs <- predict(c50fit, newdata = testing, type = "prob")
c50Probs
confusionMatrix(c50Classes, testing.class)

c50Roc <- roc(testing.class,c50Probs[,"deceptive"], levels = c("trustworthy","deceptive"))
c50Roc
plot(c50Roc, print.thres="best", print.thres.best.method="closest.topleft")
c50RocCoords <- coords(  c50Roc, "best", best.method="closest.topleft",
                         ret=c("threshold", "accuracy"))
c50RocCoords

rm(c50Roc, c50RocCoords, c50Probs, c50Classes, c50Importance)


#How do these models compare in terms of their resampling results? The resamples function can be
#used to collect, summarize and contrast the resampling results. Since the random number seeds
#were initialized to the same value prior to calling train, the same folds were used for each model.
resamps <- resamples(list(pls = plsFit, rda = rdaFit, svm = svmFit, rf = rfFit, knn = knnFit, c50 = c50fit))
summary(resamps)
xyplot(resamps, what = "BlandAltman") 

#Since, for each resample, there are paired results a paired t–test can be used to assess whether there
#is a difference in the average resampled area under the ROC curve. The diff.resamples function
#can be used to compute this:
diffs <- diff(resamps)
summary(diffs)

