library(caret)

set.seed(2969)
imbal_train <- twoClassSim(10000, intercept = -20, linearVars = 20)
imbal_test  <- twoClassSim(10000, intercept = -20, linearVars = 20)
table(imbal_train$Class)
## 
## Class1 Class2 
##   9411    589

set.seed(9560)
down_train <- downSample(x = imbal_train[, -ncol(imbal_train)],
                         y = imbal_train$Class)
table(down_train$Class)   
## 
## Class1 Class2 
##    589    589

set.seed(9560)
up_train <- upSample(x = imbal_train[, -ncol(imbal_train)],
                     y = imbal_train$Class)                         
table(up_train$Class) 
## 
## Class1 Class2 
##   9411   9411

library(DMwR)
set.seed(9560)
smote_train <- SMOTE(Class ~ ., data  = imbal_train)                         
table(smote_train$Class) 
## 
## Class1 Class2 
##   2356   1767

library(ROSE)
set.seed(9560)
rose_train <- ROSE(Class ~ ., data  = imbal_train)$data                         
table(rose_train$Class) 
## 
## Class1 Class2 
##   4939   5061


ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

set.seed(5627)
orig_fit <- train(Class ~ ., data = imbal_train, 
                  method = "treebag",
                  nbagg = 50,
                  metric = "ROC",
                  trControl = ctrl)

set.seed(5627)
down_outside <- train(Class ~ ., data = down_train, 
                      method = "treebag",
                      nbagg = 50,
                      metric = "ROC",
                      trControl = ctrl)

set.seed(5627)
up_outside <- train(Class ~ ., data = up_train, 
                    method = "treebag",
                    nbagg = 50,
                    metric = "ROC",
                    trControl = ctrl)

set.seed(5627)
rose_outside <- train(Class ~ ., data = rose_train, 
                      method = "treebag",
                      nbagg = 50,
                      metric = "ROC",
                      trControl = ctrl)


set.seed(5627)
smote_outside <- train(Class ~ ., data = smote_train, 
                       method = "treebag",
                       nbagg = 50,
                       metric = "ROC",
                       trControl = ctrl)


outside_models <- list(original = orig_fit,
                       down = down_outside,
                       up = up_outside,
                       SMOTE = smote_outside,
                       ROSE = rose_outside)

outside_resampling <- resamples(outside_models)

test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$Class, 
                 predict(model, data, type = "prob")[, "Class1"],
                 levels = c("Class2", "Class1"))
  ci(roc_obj)
}

outside_test <- lapply(outside_models, test_roc, data = imbal_test)
outside_test <- lapply(outside_test, as.vector)
outside_test <- do.call("rbind", outside_test)
colnames(outside_test) <- c("lower", "ROC", "upper")
outside_test <- as.data.frame(outside_test)

summary(outside_resampling, metric = "ROC")


## 
## Call:
## summary.resamples(object = outside_resampling, metric = "ROC")
## 
## Models: original, down, up, SMOTE, ROSE 
## Number of resamples: 50 
## 
## ROC 
##            Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
## original 0.8935  0.9311 0.9407 0.9410  0.9564 0.9717    0
## down     0.8937  0.9205 0.9369 0.9347  0.9549 0.9684    0
## up       0.9995  1.0000 1.0000 0.9999  1.0000 1.0000    0
## SMOTE    0.9622  0.9762 0.9808 0.9796  0.9840 0.9925    0
## ROSE     0.8764  0.8908 0.8953 0.8956  0.8999 0.9166    0
outside_test
##              lower       ROC     upper
## original 0.9134895 0.9251843 0.9368791
## down     0.9238817 0.9315572 0.9392327
## up       0.9360997 0.9437082 0.9513167
## SMOTE    0.9390808 0.9457264 0.9523720
## ROSE     0.9409081 0.9474742 0.9540403



The way to enable subsampling is to use yet another option in trainControl called sampling. The most basic 
syntax is to use a character string with the name of the sampling method, either "down", "up", "smote", or 
"rose". Note that you will need to have the DMwR and ROSE packages installed to use SMOTE and ROSE, 
respectively.



ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     ## new option here:
                     sampling = "down")

set.seed(5627)
down_inside <- train(Class ~ ., data = imbal_train,
                     method = "treebag",
                     nbagg = 50,
                     metric = "ROC",
                     trControl = ctrl)

## now just change that option
ctrl$sampling <- "up"

set.seed(5627)
up_inside <- train(Class ~ ., data = imbal_train,
                   method = "treebag",
                   nbagg = 50,
                   metric = "ROC",
                   trControl = ctrl)

ctrl$sampling <- "rose"

set.seed(5627)
rose_inside <- train(Class ~ ., data = imbal_train,
                     method = "treebag",
                     nbagg = 50,
                     metric = "ROC",
                     trControl = ctrl)

ctrl$sampling <- "smote"

set.seed(5627)
smote_inside <- train(Class ~ ., data = imbal_train,
                      method = "treebag",
                      nbagg = 50,
                      metric = "ROC",
                      trControl = ctrl)

inside_models <- list(original = orig_fit,
                      down = down_inside,
                      up = up_inside,
                      SMOTE = smote_inside,
                      ROSE = rose_inside)

inside_resampling <- resamples(inside_models)

inside_test <- lapply(inside_models, test_roc, data = imbal_test)
inside_test <- lapply(inside_test, as.vector)
inside_test <- do.call("rbind", inside_test)
colnames(inside_test) <- c("lower", "ROC", "upper")
inside_test <- as.data.frame(inside_test)

summary(inside_resampling, metric = "ROC")
## 
## Call:
## summary.resamples(object = inside_resampling, metric = "ROC")
## 
## Models: original, down, up, SMOTE, ROSE 
## Number of resamples: 50 
## 
## ROC 
##            Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
## original 0.8935  0.9311 0.9407 0.9410  0.9564 0.9717    0
## down     0.8902  0.9348 0.9445 0.9421  0.9507 0.9665    0
## up       0.8928  0.9211 0.9374 0.9352  0.9499 0.9637    0
## SMOTE    0.9299  0.9458 0.9528 0.9527  0.9609 0.9761    0
## ROSE     0.9189  0.9440 0.9529 0.9517  0.9597 0.9741    0
inside_test
##              lower       ROC     upper
## original 0.9134895 0.9251843 0.9368791
## down     0.9234409 0.9308037 0.9381665
## up       0.9241506 0.9340749 0.9439992
## SMOTE    0.9455682 0.9512705 0.9569727
## ROSE     0.9413039 0.9482492 0.9551946