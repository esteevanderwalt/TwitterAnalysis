# Experiment results - Engineered features










```
## Loading required package: DBI
```

```
## [1] TRUE
```



##Pre-processing

```r
suppressMessages(library(reshape2))

ggplot_missing <- function(x) {
    
    x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) + geom_raster(aes(fill = value)) + 
        scale_fill_grey(name = "", labels = c("Present", "Missing")) + theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + labs(x = "Variables in Dataset", 
        y = "Rows / observations")
}
ggplot_missing(users1.data)
```

![](Results_engineered_files/figure-html/preprocessing-1.png)<!-- -->

```r
# PREPROCESSING OF DATA first replace NA with other
users1.data$sentiment[is.na(users1.data$sentiment)] <- "Other"
users1.data$emotion[is.na(users1.data$emotion)] <- "Other"
users1.data$distance_location[is.na(users1.data$distance_location)] <- 0
users1.data$distance_tz[is.na(users1.data$distance_tz)] <- 0
users1.data$continent[is.na(users1.data$continent)] <- "Other"
users1.data$sub_region[is.na(users1.data$sub_region)] <- "Other"
users1.data$gender[is.na(users1.data$gender)] <- "Other"
users1.data$avg_tweet_time[is.na(users1.data$avg_tweet_time)] <- 12
users1.data$no_of_devices[is.na(users1.data$no_of_devices)] <- 1
users1.data$levenshtein[is.na(users1.data$levenshtein)] <- 1
users1.data$hamming[is.na(users1.data$hamming)] <- 1
users1.data$valid_name[is.na(users1.data$valid_name)] <- 0
users1.data$image_gender[is.na(users1.data$image_gender)] <- "Other"
users1.data$image_age[is.na(users1.data$image_age)] <- 20
users1.data$no_of_faces[is.na(users1.data$image_age)] <- 20

# change last tweet time
users1.data$last_tweet_time <- year(ymd_hms(users1.data$last_tweet_time))
users1.data$last_tweet_time[is.na(users1.data$last_tweet_time)] <- 2000

# change location, language, timezone to only have top50 and other
d <- users1.data %>% group_by(continent) %>% summarise(n = n()) %>% arrange(desc(n))
l <- subset(users1.data, !(continent %in% d$continent[1:50]))$continent
users1.data$continent[users1.data$continent %in% l] <- "Other"
rm(d, l)

# remove decimals from numerics
users1.data$image_age <- round(users1.data$image_age)
users1.data$avg_tweet_time <- round(users1.data$avg_tweet_time)

# update name
users1.data[users1.data$valid_name != 0, ]$valid_name <- 1

ggplot_missing(users1.data)
```

![](Results_engineered_files/figure-html/preprocessing-2.png)<!-- -->

```r
# select only those attributes worthy of ML
myvars <- c("distance_location", "distance_tz", "continent", "gender", "avg_tweet_time", 
    "no_of_devices", "levenshtein", "hamming", "valid_name", "image_gender", 
    "image_age", "no_of_faces", "last_tweet_time", "class")
ml1.full <- users1.data[myvars]
rm(myvars)
# identify nonzero attributes that can influence result
nzv <- nearZeroVar(ml1.full, saveMetrics = TRUE)
nzv[nzv$nzv, ]
```

```
##                     freqRatio percentUnique zeroVar  nzv
## distance_location 24563.00000   1.179150476   FALSE TRUE
## distance_tz         358.92737   5.093126089   FALSE TRUE
## image_age            32.26165   0.123274822   FALSE TRUE
## class                73.63000   0.002679887   FALSE TRUE
```

```r
rm(nzv)

# create dummy vars
dmy <- dummyVars("class ~ .", data = ml1.full, fullRank = T)
ml1 <- data.frame(predict(dmy, newdata = ml1.full))
rm(dmy)

# identify correlated predictors
descrCor <- cor(ml1)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > 0.999)
highCorr
```

```
## [1] NA
```

```r
rm(descrCor, highCorr)
```


```r
set.seed(123)
inTrain <- createDataPartition(y = ml1.full$class, p = 0.75, list = FALSE)
# str(inTrain)

training <- ml1[inTrain, ]
testing <- ml1[-inTrain, ]
training.class <- ml1.full[inTrain, ]$class
testing.class <- ml1.full[-inTrain, ]$class
# nrow(training) nrow(testing)
rm(inTrain)

ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, 
    summaryFunction = twoClassSummary)
```

##PLS - Partial Lease Squares

```r
# cl <- makeCluster(detectCores()) registerDoParallel(cl)
plsFit <- train(x = training, y = training.class, method = "pls", tuneLength = 15, 
    trControl = ctrl, metric = "ROC", preProc = c("center", "scale"))
```

```
## Loading required package: pls
```

```
## 
## Attaching package: 'pls'
```

```
## The following object is masked from 'package:stats':
## 
##     loadings
```

```r
save(plsFit, file = "eplsFit.RData")

# load('eplsFit.RData') plsFit <- eplsFit rm(eplsFit) stopCluster(cl)
# registerDoSEQ()

# show result
plsFit
```

```
## Partial Least Squares 
## 
## 55973 samples
##    25 predictor
##     2 classes: 'deceptive', 'trustworthy' 
## 
## Pre-processing: centered (25), scaled (25) 
## Resampling: Cross-Validated (10 fold, repeated 3 times) 
## Summary of sample sizes: 50376, 50375, 50376, 50375, 50376, 50376, ... 
## Resampling results across tuning parameters:
## 
##   ncomp  ROC        Sens  Spec
##    1     0.2006954  NaN   NaN 
##    2     0.2006954  NaN   NaN 
##    3     0.2006954  NaN   NaN 
##    4     0.2006954  NaN   NaN 
##    5     0.2006954  NaN   NaN 
##    6     0.2006954  NaN   NaN 
##    7     0.2006954  NaN   NaN 
##    8     0.2006954  NaN   NaN 
##    9     0.2006954  NaN   NaN 
##   10     0.2006954  NaN   NaN 
##   11     0.2006954  NaN   NaN 
##   12     0.2006954  NaN   NaN 
##   13     0.2006954  NaN   NaN 
##   14     0.2006954  NaN   NaN 
##   15     0.2006954  NaN   NaN 
## 
## ROC was used to select the optimal model using  the largest value.
## The final value used for the model was ncomp = 1.
```

```r
plot(plsFit)
```

![](Results_engineered_files/figure-html/pls-1.png)<!-- -->

```r
# plot(plsFit, metric='Kappa')
ggplot(plsFit)
```

![](Results_engineered_files/figure-html/pls-2.png)<!-- -->

```r
plsImportance <- varImp(plsFit, scale = FALSE)
plot(plsImportance)
```

![](Results_engineered_files/figure-html/pls-3.png)<!-- -->

```r
# predict new values
plsClasses <- predict(plsFit, newdata = testing)
```

```
## Error in object$obsLevels[apply(tmpPred[, , i, drop = FALSE], 1, which.max)]: invalid subscript type 'list'
```

```r
head(plsClasses)
```

```
## Error in head(plsClasses): object 'plsClasses' not found
```

```r
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)
```

```
##    deceptive trustworthy
## 1         NA          NA
## 8  0.2937028   0.7062972
## 10 0.2817918   0.7182082
## 18        NA          NA
## 21 0.2599441   0.7400559
## 22        NA          NA
```

```r
confusionMatrix(data = plsClasses, testing.class)
```

```
## Error in confusionMatrix(data = plsClasses, testing.class): object 'plsClasses' not found
```

```r
# ROC you could look at all predictions over all partitions and resamples at
# once: head(plsFit$pred) plot(roc(predictor = plsFit$pred$class, response =
# plsFit$pred$obs)) or for each sample library(plyr)
# l_ply(split(plsFit$pred, plsFit$pred$Resample), function(d) {
# plot(roc(predictor = d$class, response = d$obs)) })

# library(ggplot2) library(plotROC) selectedIndices <- rfFit$pred$mtry == 2
# ggplot(plsFit$pred[selectedIndices, ], aes(m = class, d = factor(obs,
# levels = c('trustworthy','deceptive')))) + geom_roc(hjust = -0.4, vjust =
# 1.5) + coord_equal()
plsRoc <- roc(testing.class, plsProbs[, "deceptive"], levels = c("trustworthy", 
    "deceptive"))
plsRoc
```

```
## 
## Call:
## roc.default(response = testing.class, predictor = plsProbs[,     "deceptive"], levels = c("trustworthy", "deceptive"))
## 
## Data: plsProbs[, "deceptive"] in 3655 controls (testing.class trustworthy) < 250 cases (testing.class deceptive).
## Area under the curve: 1
```

```r
plot(plsRoc, print.thres = "best", print.thres.best.method = "closest.topleft")
```

![](Results_engineered_files/figure-html/pls-4.png)<!-- -->

```
## 
## Call:
## roc.default(response = testing.class, predictor = plsProbs[,     "deceptive"], levels = c("trustworthy", "deceptive"))
## 
## Data: plsProbs[, "deceptive"] in 3655 controls (testing.class trustworthy) < 250 cases (testing.class deceptive).
## Area under the curve: 1
```

```r
plsRocCoords <- coords(plsRoc, "best", best.method = "closest.topleft", ret = c("threshold", 
    "accuracy"))
plsRocCoords
```

```
## threshold  accuracy 
## 0.4394718 1.0000000
```

```r
rm(plsRoc, plsRocCoords, plsProbs, plsClasses, plsImportance)
```

##RDA - Regularized Discriminant Analysis

```r
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)

rdaFit <- train(x = training, y = training.class, method = "rda", tuneGrid = rdaGrid, 
    trControl = ctrl, metric = "ROC")
```

```
## Loading required package: klaR
```

```
## Loading required package: MASS
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
save(rdaFit, file = "erdaFit.RData")

# load('erdaFit.RData') rdaFit <- erdaFit rm(erdaFit)
rdaFit
```

```
## Regularized Discriminant Analysis 
## 
## 55973 samples
##    25 predictor
##     2 classes: 'deceptive', 'trustworthy' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 3 times) 
## Summary of sample sizes: 50376, 50375, 50375, 50376, 50376, 50376, ... 
## Resampling results across tuning parameters:
## 
##   gamma  ROC  Sens  Spec
##   0.00   1    0     1   
##   0.25   1    0     1   
##   0.50   1    0     1   
##   0.75   1    0     1   
##   1.00   1    0     1   
## 
## Tuning parameter 'lambda' was held constant at a value of 0.75
## ROC was used to select the optimal model using  the largest value.
## The final values used for the model were gamma = 0 and lambda = 0.75.
```

```r
plot(rdaFit)
```

![](Results_engineered_files/figure-html/rda-1.png)<!-- -->

```r
rdaImportance <- varImp(rdaFit, scale = FALSE)
plot(rdaImportance)
```

![](Results_engineered_files/figure-html/rda-2.png)<!-- -->

```r
rdaClasses <- predict(rdaFit, newdata = testing)
rdaProbs <- predict(rdaFit, newdata = testing, type = "prob")
confusionMatrix(rdaClasses, testing.class)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    deceptive trustworthy
##   deceptive           0           0
##   trustworthy       250       18407
##                                           
##                Accuracy : 0.9866          
##                  95% CI : (0.9848, 0.9882)
##     No Information Rate : 0.9866          
##     P-Value [Acc > NIR] : 0.5168          
##                                           
##                   Kappa : 0               
##  Mcnemar's Test P-Value : <2e-16          
##                                           
##             Sensitivity : 0.0000          
##             Specificity : 1.0000          
##          Pos Pred Value :    NaN          
##          Neg Pred Value : 0.9866          
##              Prevalence : 0.0134          
##          Detection Rate : 0.0000          
##    Detection Prevalence : 0.0000          
##       Balanced Accuracy : 0.5000          
##                                           
##        'Positive' Class : deceptive       
## 
```

```r
rdaRoc <- roc(testing.class, rdaProbs[, "deceptive"], levels = c("trustworthy", 
    "deceptive"))
```

```
## Error in roc.default(testing.class, rdaProbs[, "deceptive"], levels = c("trustworthy", : No control observation.
```

```r
rdaRoc
```

```
## Error in eval(expr, envir, enclos): object 'rdaRoc' not found
```

```r
plot(rdaRoc, print.thres = "best", print.thres.best.method = "closest.topleft")
```

```
## Error in plot(rdaRoc, print.thres = "best", print.thres.best.method = "closest.topleft"): object 'rdaRoc' not found
```

```r
rdaRocCoords <- coords(rdaRoc, "best", best.method = "closest.topleft", ret = c("threshold", 
    "accuracy"))
```

```
## Error in coords(rdaRoc, "best", best.method = "closest.topleft", ret = c("threshold", : object 'rdaRoc' not found
```

```r
rdaRocCoords
```

```
## Error in eval(expr, envir, enclos): object 'rdaRocCoords' not found
```

```r
rm(rdaRoc, rdaRocCoords, rdaProbs, rdaClasses, rdaImportance)
```

###Linear SVM

```r
set.seed(123)

svmFit <- train(x = training, y = training.class,
                method = "svmLinear",
                #weights = weights,
                maximize = T,
                tuneGrid = expand.grid(.C=3^(-15:15)),
                trControl = ctrl,
                metric = "ROC")
```

```
## Loading required package: kernlab
```

```
## 
## Attaching package: 'kernlab'
```

```
## The following object is masked from 'package:scales':
## 
##     alpha
```

```
## The following object is masked from 'package:ggplot2':
## 
##     alpha
```

```r
save(svmFit,file="esvmFit.RData")

#load("esvmFit.RData")
#svmFit <- esvmFit
#rm(esvmFit)

svmFit
```

```
## Support Vector Machines with Linear Kernel 
## 
## 55973 samples
##    25 predictor
##     2 classes: 'deceptive', 'trustworthy' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 3 times) 
## Summary of sample sizes: 50376, 50375, 50375, 50376, 50376, 50376, ... 
## Resampling results across tuning parameters:
## 
##   C             ROC   Sens  Spec
##   6.969172e-08  0.75  NaN   NaN 
##   2.090752e-07  0.75  NaN   NaN 
##   6.272255e-07  0.75  NaN   NaN 
##   1.881676e-06  0.75  NaN   NaN 
##   5.645029e-06  0.75  NaN   NaN 
##   1.693509e-05  0.75  NaN   NaN 
##   5.080526e-05  0.75  NaN   NaN 
##   1.524158e-04  0.75  NaN   NaN 
##   4.572474e-04  0.75  NaN   NaN 
##   1.371742e-03  0.75  NaN   NaN 
##   4.115226e-03  0.75  NaN   NaN 
##   1.234568e-02  0.75  NaN   NaN 
##   3.703704e-02  0.75  NaN   NaN 
##   1.111111e-01  0.75  NaN   NaN 
##   3.333333e-01  0.75  NaN   NaN 
##   1.000000e+00  0.75  NaN   NaN 
##   3.000000e+00  0.75  NaN   NaN 
##   9.000000e+00  0.75  NaN   NaN 
##   2.700000e+01  0.75  NaN   NaN 
##   8.100000e+01  0.75  NaN   NaN 
##   2.430000e+02  0.75  NaN   NaN 
##   7.290000e+02  0.75  NaN   NaN 
##   2.187000e+03  0.75  NaN   NaN 
##   6.561000e+03  0.75  NaN   NaN 
##   1.968300e+04  0.75  NaN   NaN 
##   5.904900e+04  0.75  NaN   NaN 
##   1.771470e+05  0.75  NaN   NaN 
##   5.314410e+05  0.75  NaN   NaN 
##   1.594323e+06  0.75  NaN   NaN 
##   4.782969e+06  0.75  NaN   NaN 
##   1.434891e+07  0.75  NaN   NaN 
## 
## ROC was used to select the optimal model using  the largest value.
## The final value used for the model was C = 6.969172e-08.
```

```r
plot(svmFit)
```

![](Results_engineered_files/figure-html/linearsvm-1.png)<!-- -->

```r
svmImportance <- varImp(svmFit, scale=FALSE)
plot(svmImportance)
```

![](Results_engineered_files/figure-html/linearsvm-2.png)<!-- -->

```r
svmClasses <- predict(svmFit, newdata = testing)
head(svmClasses)
```

```
## [1] <NA> <NA> <NA> <NA> <NA> <NA>
## Levels: deceptive trustworthy
```

```r
svmProbs <- predict(svmFit, newdata = testing, type = "prob")
head(svmProbs)
```

```
##   deceptive trustworthy
## 1        NA          NA
## 2        NA          NA
## 3        NA          NA
## 4        NA          NA
## 5        NA          NA
## 6        NA          NA
```

```r
confusionMatrix(svmClasses, testing.class)
```

```
## Error in binom.test(sum(diag(x)), sum(x)): 'n' must be a positive integer >= 'x'
```

```r
svmRoc <- roc(testing.class,svmProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
```

```
## Error in roc.default(testing.class, svmProbs[, "deceptive"], levels = c("trustworthy", : Predictor must be numeric or ordered.
```

```r
svmRoc
```

```
## Error in eval(expr, envir, enclos): object 'svmRoc' not found
```

```r
plot(svmRoc, print.thres="best", print.thres.best.method="closest.topleft")
```

```
## Error in plot(svmRoc, print.thres = "best", print.thres.best.method = "closest.topleft"): object 'svmRoc' not found
```

```r
svmRocCoords <- coords(  svmRoc, "best", best.method="closest.topleft",
                         ret=c("threshold", "accuracy"))
```

```
## Error in coords(svmRoc, "best", best.method = "closest.topleft", ret = c("threshold", : object 'svmRoc' not found
```

```r
svmRocCoords
```

```
## Error in eval(expr, envir, enclos): object 'svmRocCoords' not found
```

```r
rm(svmRoc, svmRocCoords, svmProbs, svmClasses, svmImportance)
```

##RF - Random Forest

```r
set.seed(123)
rfFit <- train(x = training, y = training.class, method = "rf", trControl = ctrl, 
    metric = "ROC", preProcess = c("center", "scale"), tuneLength = 20)
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## Error in randomForest.default(x, y, mtry = param$mtry, ...): NA not permitted in predictors
```

```
## Timing stopped at: 0.46 0.05 0.5
```

```r
save(rfFit, file = "erfFit.RData")
```

```
## Error in save(rfFit, file = "erfFit.RData"): object 'rfFit' not found
```

```r
# load('erfFit.RData') rfFit <- erfFit rm(erfFit)

rfFit
```

```
## Error in eval(expr, envir, enclos): object 'rfFit' not found
```

```r
plot(rfFit)
```

```
## Error in plot(rfFit): object 'rfFit' not found
```

```r
# list the chosen features
predictors(rfFit)
```

```
## Error in predictors(rfFit): object 'rfFit' not found
```

```r
# plot the results
plot(rfFit, type = c("g", "o"))
```

```
## Error in plot(rfFit, type = c("g", "o")): object 'rfFit' not found
```

```r
rfImportance <- varImp(rfFit, scale = FALSE)
```

```
## Error in varImp(rfFit, scale = FALSE): object 'rfFit' not found
```

```r
plot(rfImportance)
```

```
## Error in plot(rfImportance): object 'rfImportance' not found
```

```r
rfClasses <- predict(rfFit, newdata = testing)
```

```
## Error in predict(rfFit, newdata = testing): object 'rfFit' not found
```

```r
head(rfClasses)
```

```
## Error in head(rfClasses): object 'rfClasses' not found
```

```r
rfProbs <- predict(rfFit, newdata = testing, type = "prob")
```

```
## Error in predict(rfFit, newdata = testing, type = "prob"): object 'rfFit' not found
```

```r
head(rfProbs)
```

```
## Error in head(rfProbs): object 'rfProbs' not found
```

```r
confusionMatrix(rfClasses, testing.class)
```

```
## Error in confusionMatrix(rfClasses, testing.class): object 'rfClasses' not found
```

```r
rfRoc <- roc(testing.class, rfProbs[, "deceptive"], levels = c("trustworthy", 
    "deceptive"))
```

```
## Error in roc.default(testing.class, rfProbs[, "deceptive"], levels = c("trustworthy", : object 'rfProbs' not found
```

```r
rfRoc
```

```
## Error in eval(expr, envir, enclos): object 'rfRoc' not found
```

```r
plot(rfRoc, print.thres = "best", print.thres.best.method = "closest.topleft")
```

```
## Error in plot(rfRoc, print.thres = "best", print.thres.best.method = "closest.topleft"): object 'rfRoc' not found
```

```r
rfRocCoords <- coords(rfRoc, "best", best.method = "closest.topleft", ret = c("threshold", 
    "accuracy"))
```

```
## Error in coords(rfRoc, "best", best.method = "closest.topleft", ret = c("threshold", : object 'rfRoc' not found
```

```r
rfRocCoords
```

```
## Error in eval(expr, envir, enclos): object 'rfRocCoords' not found
```

```r
rm(rfRoc, rfRocCoords, rfProbs, rfClasses, rfImportance)
```

##KNN - K-Nearest Neighbour

```r
set.seed(123)
knnFit <- train(x = training, y = training.class, method = "kknn", trControl = ctrl, 
    metric = "ROC", preProcess = c("center", "scale"), tuneLength = 20)
```

```
## 1 package is needed for this model and is not installed. (kknn). Would you like to try to install it now?
```

```
## Error in checkInstall(models$library):
```

```r
save(knnFit, file = "eknnFit.RData")
```

```
## Error in save(knnFit, file = "eknnFit.RData"): object 'knnFit' not found
```

```r
# load('eknnFit.RData') knnFit <- eknnFit rm(eknnFit)

knnFit
```

```
## Error in eval(expr, envir, enclos): object 'knnFit' not found
```

```r
plot(knnFit)
```

```
## Error in plot(knnFit): object 'knnFit' not found
```

```r
knnImportance <- varImp(knnFit, scale = FALSE)
```

```
## Error in varImp(knnFit, scale = FALSE): object 'knnFit' not found
```

```r
plot(knnImportance)
```

```
## Error in plot(knnImportance): object 'knnImportance' not found
```

```r
knnClasses <- predict(knnFit, newdata = testing)
```

```
## Error in predict(knnFit, newdata = testing): object 'knnFit' not found
```

```r
head(knnClasses)
```

```
## Error in head(knnClasses): object 'knnClasses' not found
```

```r
knnProbs <- predict(knnFit, newdata = testing, type = "prob")
```

```
## Error in predict(knnFit, newdata = testing, type = "prob"): object 'knnFit' not found
```

```r
head(knnProbs)
```

```
## Error in head(knnProbs): object 'knnProbs' not found
```

```r
confusionMatrix(knnClasses, testing.class)
```

```
## Error in confusionMatrix(knnClasses, testing.class): object 'knnClasses' not found
```

```r
knnRoc <- roc(testing.class, knnProbs[, "deceptive"], levels = c("trustworthy", 
    "deceptive"))
```

```
## Error in roc.default(testing.class, knnProbs[, "deceptive"], levels = c("trustworthy", : object 'knnProbs' not found
```

```r
knnRoc
```

```
## Error in eval(expr, envir, enclos): object 'knnRoc' not found
```

```r
plot(knnRoc, print.thres = "best", print.thres.best.method = "closest.topleft")
```

```
## Error in plot(knnRoc, print.thres = "best", print.thres.best.method = "closest.topleft"): object 'knnRoc' not found
```

```r
knnRocCoords <- coords(knnRoc, "best", best.method = "closest.topleft", ret = c("threshold", 
    "accuracy"))
```

```
## Error in coords(knnRoc, "best", best.method = "closest.topleft", ret = c("threshold", : object 'knnRoc' not found
```

```r
knnRocCoords
```

```
## Error in eval(expr, envir, enclos): object 'knnRocCoords' not found
```

```r
rm(knnRoc, knnRocCoords, knnProbs, knnClasses, knnImportance)
```

##C5.0 - Boosting

```r
set.seed(123)
c50fit <- train(x = training, y = training.class, method = "C5.0", metric = "ROC", 
    trControl = control, verbose = FALSE)
```

```
## Loading required package: C50
```

```
## Loading required package: plyr
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following object is masked from 'package:lubridate':
## 
##     here
```

```
## Error in train.default(x = training, y = training.class, method = "C5.0", : object 'control' not found
```

```r
save(c50fit, file = "ec50fit.RData")
```

```
## Error in save(c50fit, file = "ec50fit.RData"): object 'c50fit' not found
```

```r
# load('ec50Fit.RData') c50Fit <- ec50Fit rm(ec50Fit)

c50fit
```

```
## Error in eval(expr, envir, enclos): object 'c50fit' not found
```

```r
# visualize the resample distributions
xyplot(c50fit, type = c("g", "p", "smooth"))
```

```
## Error in xyplot(c50fit, type = c("g", "p", "smooth")): object 'c50fit' not found
```

```r
plot(c50fit)
```

```
## Error in plot(c50fit): object 'c50fit' not found
```

```r
c50Importance <- varImp(c50fit, scale = FALSE)
```

```
## Error in varImp(c50fit, scale = FALSE): object 'c50fit' not found
```

```r
plot(c50Importance)
```

```
## Error in plot(c50Importance): object 'c50Importance' not found
```

```r
c50Classes <- predict(c50fit, newdata = testing)
```

```
## Error in predict(c50fit, newdata = testing): object 'c50fit' not found
```

```r
head(c50Classes)
```

```
## Error in head(c50Classes): object 'c50Classes' not found
```

```r
c50Probs <- predict(c50fit, newdata = testing, type = "prob")
```

```
## Error in predict(c50fit, newdata = testing, type = "prob"): object 'c50fit' not found
```

```r
head(c50Probs)
```

```
## Error in head(c50Probs): object 'c50Probs' not found
```

```r
confusionMatrix(c50Classes, testing.class)
```

```
## Error in confusionMatrix(c50Classes, testing.class): object 'c50Classes' not found
```

```r
c50Roc <- roc(testing.class, c50Probs[, "deceptive"], levels = c("trustworthy", 
    "deceptive"))
```

```
## Error in roc.default(testing.class, c50Probs[, "deceptive"], levels = c("trustworthy", : object 'c50Probs' not found
```

```r
c50Roc
```

```
## Error in eval(expr, envir, enclos): object 'c50Roc' not found
```

```r
plot(c50Roc, print.thres = "best", print.thres.best.method = "closest.topleft")
```

```
## Error in plot(c50Roc, print.thres = "best", print.thres.best.method = "closest.topleft"): object 'c50Roc' not found
```

```r
c50RocCoords <- coords(c50Roc, "best", best.method = "closest.topleft", ret = c("threshold", 
    "accuracy"))
```

```
## Error in coords(c50Roc, "best", best.method = "closest.topleft", ret = c("threshold", : object 'c50Roc' not found
```

```r
c50RocCoords
```

```
## Error in eval(expr, envir, enclos): object 'c50RocCoords' not found
```

```r
rm(c50Roc, c50RocCoords, c50Probs, c50Classes, c50Importance)
```

###################
##Final comparison
###################

```r
# How do these models compare in terms of their resampling results? The
# resamples function can be used to collect, summarize and contrast the
# resampling results. Since the random number seeds were initialized to the
# same value prior to calling train, the same folds were used for each
# model.
resamps <- resamples(list(pls = plsFit, rda = rdaFit, svm = svmFit, rf = rfFit, 
    knn = knnFit, c50 = c50fit))
```

```
## Error in resamples(list(pls = plsFit, rda = rdaFit, svm = svmFit, rf = rfFit, : object 'rfFit' not found
```

```r
summary(resamps)
```

```
## Error in summary(resamps): object 'resamps' not found
```

```r
xyplot(resamps, what = "BlandAltman")
```

```
## Error in xyplot(resamps, what = "BlandAltman"): object 'resamps' not found
```

```r
# Since, for each resample, there are paired results a paired t–test can be
# used to assess whether there is a difference in the average resampled area
# under the ROC curve. The diff.resamples function can be used to compute
# this:
diffs <- diff(resamps)
```

```
## Error in diff(resamps): object 'resamps' not found
```

```r
summary(diffs)
```

```
## Error in summary(diffs): object 'diffs' not found
```
