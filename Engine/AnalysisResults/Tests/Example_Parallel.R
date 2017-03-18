library(doParallel); library(caret)

#create a list of seed, here change the seed for each resampling
set.seed(123)

#length is = (n_repeats*nresampling)+1
seeds <- vector(mode = "list", length = 11)

#(3 is the number of tuning parameter, mtry for rf, here equal to ncol(iris)-2)
for(i in 1:10) seeds[[i]]<- sample.int(n=1000, 3)

#for the last model
seeds[[11]]<-sample.int(1000, 1)

#control list
myControl <- trainControl(method='cv', seeds=seeds, index=createFolds(iris$Species))

#run model in parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)
model1 <- train(Species~., iris, method='rf', trControl=myControl)

model2 <- train(Species~., iris, method='rf', trControl=myControl)
stopCluster(cl)

#compare
all.equal(predict(model1, type='prob'), predict(model2, type='prob'))
[1] TRUE