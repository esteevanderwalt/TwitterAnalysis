## library to hold common functions used in the IDI engine

#CHECK MISSING DATA
# A function that plots missingness

## @knitr part_libs
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


####################################
##  FUNCTION for partial least squares
####################################
runModel <- function(x, s, m, tlength, saved){
  set.seed(123)
  
  print("Create partition")
  inTrain <- createDataPartition(y = x$class, p = .75, list = FALSE)
  #str(inTrain)
  
  training <- x[inTrain,]
  testing <- x[-inTrain,]
  rm(inTrain)  
  
  print("Run model")
  ctrl <- trainControl(method = "repeatedcv", 
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
  
  #cl <- makeCluster(detectCores())
  #registerDoParallel(cl)
  s <- paste(s,m,"Fit.RData", sep="_")
  if(saved == 0){
    mFit <- train(class ~ ., data = training,
                    method = m, 
                    tuneLength = tlength, 
                    trControl = ctrl, 
                    metric = "ROC",
                    preProc = c("center", "scale"))
    save(mFit,file=s)    
  }else{
    load(s)
    #stopCluster(cl)
    #registerDoSEQ()
  }
  
  print("Show model results")
  #show result
  print(mFit)
  p <- plot(mFit)
  print(p)
  #plot(mFit, metric="Kappa")
  #ggplot(mFit)
  
  print("Show variable importance")
  mImportance <- varImp(mFit, scale=FALSE)
  print(mImportance)
  p <- plot(mImportance)
  print(p)
  
  print("Predict new values - evaluate model")
  mClasses <- predict(mFit, newdata = testing)
  print(head(mClasses))
  
  mProbs <- predict(mFit, newdata = testing, type = "prob")
  print(head(mProbs))
  
  print("Show evaluation results")
  p <- confusionMatrix(data = mClasses, testing$class)
  print(p)
  
  #ROC
  mRoc <- roc(testing$class,mProbs[,"deceptive"], levels = c("trustworthy","deceptive"))
  print(mRoc)
  p <- plot(mRoc, print.thres="best", print.thres.best.method="closest.topleft")
  print(p)
  mRocCoords <- coords(  mRoc, "best", best.method="closest.topleft",
                           ret=c("threshold", "accuracy"))
  print(mRocCoords)
  
  rm(mRoc, mRocCoords, mProbs, mClasses, mImportance)
  
  return(mFit)
}

resampling <- function(x){
  #How do these models compare in terms of their resampling results? The resamples function can be
  #used to collect, summarize and contrast the resampling results. Since the random number seeds
  #were initialized to the same value prior to calling train, the same folds were used for each model.
  resamps <- resamples(x)
  print(summary(resamps))
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