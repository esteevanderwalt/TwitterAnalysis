
library(RPostgreSQL)
library(caret)
library(lubridate)
library(doParallel)
library(pROC)

#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "twitter",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "")
  #rm(pw) # removes the password

users.all <- dbGetQuery(con, "SELECT * from main.zz_full_set") 
myvars <- c("name", "screenname", "location", "language", "timezone", "utc_offset",
            "geo_enabled", "latitude", "longitude", "profile_image", 
            "is_default_profile", "is_default_profile_image", "created", "class")
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
#first replace NA with other
users1.data$timezone[is.na(users1.data$timezone)] <- 'Other'
users1.data$latitude[is.na(users1.data$latitude)] <- 0
users1.data$longitude[is.na(users1.data$longitude)] <- 0

#change created to be year of creation
users1.data$created <- year(ymd_hms(users1.data$created))
users1.data$created[is.na(users1.data$created)] <- 2000

#change location, language, timezone to only have top50 and other
suppressMessages(library(dplyr))
d <- users1.data %>% 
  group_by(location) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(users1.data, !(location %in% d$location[1:50]))$location
users1.data$location[users1.data$location %in% l] <- 'Other'
rm(d, l)

d <- users1.data %>% 
  group_by(timezone) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(users1.data, !(timezone %in% d$timezone[1:20]))$timezone
users1.data$timezone[users1.data$timezone %in% l] <- 'Other'
rm(d, l)

d <- users1.data %>% 
  group_by(language) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(users1.data, !(language %in% d$language[1:20]))$language
users1.data$language[users1.data$language %in% l] <- 'Other'
rm(d, l)

#remove decimals from lat/lon
users1.data$latitude <- round(users1.data$latitude)
users1.data$longitude <- round(users1.data$longitude)

#select only those attributes worthy of ML
myvars <- c("location", "language", "timezone", "utc_offset",
            "geo_enabled", "latitude", "longitude",  
            "is_default_profile", "is_default_profile_image", "created", "class")
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

#start training of data
set.seed(107)
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
                     savePredictions = TRUE,
                     summaryFunction = twoClassSummary)

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
plsFit <- train(x = training, y = training.class,
                method = "pls", 
                tuneLength = 15, 
                trControl = ctrl, 
                metric = "ROC",
                preProc = c("center", "scale"))
#stopCluster(cl)
#registerDoSEQ()

#show result
plsFit
plot(plsFit) 
plsImportance <- varImp(plsFit, scale=FALSE)
plot(plsImportance)

#predict new values
plsClasses <- predict(plsFit, newdata = testing)
head(plsClasses)
plsClasses

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)
plsProbs

confusionMatrix(data = plsClasses, testing.class)

#ROC
#you could look at all predictions over all partitions and resamples at once:
head(plsFit$pred)
plot(roc(predictor = plsFit$pred$class, response = plsFit$pred$obs))
#or for each sample
library(plyr)
l_ply(split(plsFit$pred, plsFit$pred$Resample), function(d) {
  plot(roc(predictor = d$class, response = d$obs))
})

library(ggplot2)
library(plotROC)
selectedIndices <- rfFit$pred$mtry == 2
ggplot(plsFit$pred[selectedIndices, ], 
       aes(m = class, d = factor(obs, levels = c("trustworthy","deceptive")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()


plsRoc <- roc(plsClasses,plsProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
plsRoc
plot(plsRoc, print.thres="best", print.thres.best.method="closest.topleft")
plsRocCoords <- coords(  plsRoc, "best", best.method="closest.topleft",
                                 ret=c("threshold", "accuracy"))
plsRocCoords

# Select a parameter setting
selectedIndices <- plsClasses == "deceptive"
# Plot:
plot.roc(plsFit$pred$obs[selectedIndices],
         plsFit$pred$M[selectedIndices])


#For example, to fit a regularized discriminant model to these data, the following syntax can be used:
## To illustrate, a custom grid is used
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)

rdaFit <- train(x = training, y = training.class,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")
rdaFit
plot(rdaFit)

rdaImportance <- varImp(rdaFit, scale=FALSE)
plot(rdaImportance)

rdaClasses <- predict(rdaFit, newdata = testing)
rdaProbs <- predict(rdaFit, newdata = testing, type = "prob")
rdaProbs
confusionMatrix(rdaClasses, testing.class)


selectedIndices <- rdaFit$pred$pred == "trustworthy"
plot.roc(rdaFit$pred$obs[selectedIndices],
         rdaFit$pred$deceptive[selectedIndices])
library(ggplot2)
library(plotROC)
ggplot(rdaFit$pred[selectedIndices, ], 
       aes(m = trustworthy, d = factor(obs, levels = c("deceptive", "trustworthy")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()


rdaRoc <- roc(testing.class,rdaProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
rdaRoc
plot(rdaRoc, print.thres="best", print.thres.best.method="closest.topleft")
rdaRocCoords <- coords(  rdaRoc, "best", best.method="closest.topleft",
                         ret=c("threshold", "accuracy"))
rdaRocCoords


#How do these models compare in terms of their resampling results? The resamples function can be
#used to collect, summarize and contrast the resampling results. Since the random number seeds
#were initialized to the same value prior to calling train, the same folds were used for each model.
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)
xyplot(resamps, what = "BlandAltman") 

#Since, for each resample, there are paired results a paired t–test can be used to assess whether there
#is a difference in the average resampled area under the ROC curve. The diff.resamples function
#can be used to compute this:
diffs <- diff(resamps)
summary(diffs)

#Random forest
#Recursive Feature Elimination or RFE
library(mlbench)
library(randomForest)
# ensure the results are repeatable
set.seed(123)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#rfFit <- train(x=training, y=ctraining, method="rf", prox=TRUE)
#rfFit
rfFit <- rfe(training, ctraining, sizes=c(1:8), rfeControl=control)
# summarize the results
print(rfFit)
# list the chosen features
predictors(rfFit)
# plot the results
plot(rfFit, type=c("g", "o"))

#Boosting
# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# C5.0
set.seed(123)
fit.c50 <- train(x=training, y=ctraining, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
#to enable parallel processing... only on unix
library(doMC)
registerDoMC(5)
##
set.seed(123)
fit.gbm <- train(x=training, y=ctraining, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)


