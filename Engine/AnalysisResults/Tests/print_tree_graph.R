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

library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")
source("LIB_Stats.R")

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

#remove NZV values
data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterFE(data.full)

#was done in cleanup
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("FF_RATIO","PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("HAMMING"))]

#perform machine learning
data.o <- data.clean

filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/rpart1.txt",sep="")
imagefilename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/rpart_",sep="")

cl <- makeCluster(detectCores())
registerDoParallel(cores=7) #or cl

set.seed(Sys.time())
inTrain <- createDataPartition(y = data.o$CLASS, p = .75, list = FALSE)

sink(filename)

cat("\n")
print("START INTRAIN")
print("==============")
print(head(inTrain))

sink()

training <- data.o[inTrain,]
testing <- data.o[-inTrain,]

fit.m7.seeds <- setSeeds("repeatedcv", 10, 3, 3)

fit.m7.fc <- trainControl(method = "repeatedcv", 
                          number = 10,
                          repeats = 3,
                          seeds = fit.m7.seeds,
                          classProbs = TRUE,
                          savePredictions = TRUE,
                          summaryFunction = get("prSummary"), #twoClassSummary,
                          sampling = "smote")

sink(filename, append = TRUE)
print("M7 started")
sink()

#print(filename)
m7.t <- system.time(fit.m7 <- train(CLASS~., data=training,
                                    method = "rpart",
                                    metric = "ROC",
                                    preProcess = c("center", "scale"),
                                    trControl = fit.m7.fc,
                                    tuneLength = 3))
runDetails(fit.m7, "fit.m7")

sink(filename, append = TRUE)
print("M7 complete")
print(m7.t)
sink()


stopCluster(cl)

plot(fit.m7$finalModel)					# Will make a mess of the plot
text(fit.m7$finalModel)
# 
prp(fit.m7$finalModel)					# Will plot the tree
prp(fit.m7$finalModel,varlen=3)				# Shorten variable names

rpart.plot(fit.m7$finalModel)

rpart.plot(fit.m7$finalModel, # middle graph
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=FALSE)


library(rattle)
fancyRpartPlot(t$finalModel)


# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle
#
#-------------------------------------------------------------------
# Plot a tree built with RevoScaleR
# Construct a model formula
sdNames <- names(segmentationData)
X <- as.vector(sdNames[-c(1,2,3)])
form <- as.formula(paste("Class","~", paste(X,collapse="+")))
# Run the model
rx.tree <- rxDTree(form, data = segmentationData,maxNumBins = 100,
                   minBucket = 10,maxDepth = 5,cp = 0.01, xVal = 0)
# Plot the tree						
prp(rxAddInheritance(rx.tree))
fancyRpartPlot(rxAddInheritance(rx.tree))