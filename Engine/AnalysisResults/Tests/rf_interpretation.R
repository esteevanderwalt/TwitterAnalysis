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
suppressMessages(library(DMwR))

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")
source("LIB_Stats.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, FOLLOWERS_COUNT, FRIENDS_COUNT, FF_RATIO, LISTED_COUNT, USERNAME_LENGTH, GEO_ENABLED, PROFILE_HAS_URL, IS_CELEBRITY, ACCOUNT_AGE_IN_MONTHS, LANGUAGE, HAS_NAME, HAS_IMAGE, DUP_PROFILE, HAS_PROFILE, STATUS_COUNT, CLASS from TWITTER.ZZ_BOT_SET") )

close(myconn)

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

#first get nzv values
nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
nzv[nzv$nzv,]

#rapply(data.full,function(x)length(unique(x)))
#rapply(d,function(x)sum(is.na(x)))

#remove NZV values
data.full <- data.full[ , -which(names(data.full) %in% c("IS_CELEBRITY"))]  #ID, SCREENNAME

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterBot(data.full)

#was done in cleanup
#remove fields not in fake accounts
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("USERNAME_LENGTH", "GEO_ENABLED"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "DUP_PROFILE", "HAS_PROFILE"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("LANGUAGE"))]

#data.clean[data.clean$FRIENDS_COUNT > 1000,]$FRIENDS_COUNT <- 1000
#data.clean[data.clean$FOLLOWERS_COUNT > 1000,]$FOLLOWERS_COUNT <- 1000
#data.clean[data.clean$STATUS_COUNT > 1000,]$STATUS_COUNT <- 1000
#data.clean[data.clean$LISTED_COUNT > 100,]$LISTED_COUNT <- 100

#perform machine learning
data.bot <- data.clean

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

#first get nzv values
nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
nzv[nzv$nzv,]

#rapply(data.full,function(x)length(unique(x)))
#rapply(d,function(x)sum(is.na(x)))

#remove NZV values
#data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterFE(data.full)

#was done in cleanup
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("FF_RATIO","PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("USERNAME_LENGTH", "GEO_ENABLED"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "DUP_PROFILE", "HAS_PROFILE"))]

data.clean <- data.clean[ , -which(names(data.clean) %in% c("HAMMING"))]

#perform machine learning
data.fe <- data.clean

#merge bot and fe data
data.o <- merge(data.bot, data.fe, by.x = c("ID","SCREENNAME","CLASS"), by.y = c("ID","SCREENNAME","CLASS"))
tmp <- data.o[,-which(names(data.o) %in% c("ID","SCREENNAME","CLASS"))]
data.o <- cbind(tmp, CLASS=data.o$CLASS)

#' ######################################
#' ### Run model and print results
#' ######################################
#+ run_models
#set.seed(123)

#inTrain <- createDataPartition(y = data.o$CLASS, p = .75, list = FALSE)

#training <- data.o[inTrain,]
#testing <- data.o[-inTrain,]
#rm(inTrain)  

getRandomData <- function(d, si){
  #split d in deceptive and not
  deceptive <- d[d$CLASS=="deceptive",]
  trustworthy <- d[d$CLASS=="trustworthy",]
  #randomly pick si (size) trustworthy
  trust <- trustworthy[sample(nrow(trustworthy), si), ]
  #add to deceptive
  total <- rbind(deceptive, trust)
  #return
  return(total)
}

################################
## Run various params on dataset
################################
resamp <- "repeatedcv"
#folds
folds <- c(10)
#repeats
repeats <- c(3)
#tune
tune <- c(3)
#sampling
sampling <- c("none")
#summary function
summF <- c("prSummary") #"twoClassSummary", 
rr <- c(1)  
sz <- c(0) #use 0 for full set - , 1000, 10000, 100000

cl <- makeCluster(detectCores())
registerDoParallel(cores=7) #or cl

    filename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/FEA3_smote20_15K_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,"_size_",s,"_round_",r,".txt",sep="")
    imagefilename <- paste("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/FEA3_smote20_15K_rcv_",x,"fold_",y,"repeat_",z,"tune_",m,"_sumf_",n,"_size_",s,"_round_",r,"_",sep="")
    
    set.seed(Sys.time())
    inTrain <- createDataPartition(y = data.o$CLASS, p = .75, list = FALSE)
    #inTrain <- sample(seq_along(data.o$CLASS), as.integer(0.75 * nrow(data.o)))
    
    
    training <- data.o[inTrain,]
    testing <- data.o[-inTrain,]
    
          
    training <- SMOTE(CLASS ~., training, perc.over=100, perc.under=200)
    #gives 45K records - reduce that to 4.5K
    inTrain <- createDataPartition(y = training$CLASS, p = .20, list = FALSE)
    training <- training[inTrain,]
    rm(inTrain)
    
    fit.m2.seeds <- setSeeds(resamp, folds, repeats, tune)
    
    fit.m2.fc <- trainControl(method = resamp, 
                              number = folds,
                              repeats = repeats,
                              seeds = fit.m2.seeds,
                              classProbs = TRUE,
                              savePredictions = TRUE,
                              summaryFunction = get(summF))
    
    # Build model
    set.seed(123)
    fit.m2 <- caret::train(CLASS~., data=training,
                                        method = "rf",
                                        metric = "ROC",
                                        preProcess = c("center", "scale"),
                                        trControl = fit.m2.fc,
                                        tuneLength = tune)
    
    runDetails(fit.m2, "fit.m2")
    
    a <- fit.m2$finalModel
    
    stopCluster(cl)
    
    library(forestFloor)
    #grow a forest, remeber to include inbag
    rfo=randomForest(CLASS~., data=training,keep.inbag = TRUE,sampsize=250,ntree=50)
    #compute topology
    ff = forestFloor(rfo,training)
    #ggPlotForestFloor(ff,1:9)
    plot(ff,1:9,col=fcol(ff),orderByImportance=TRUE)
    p <- forestFloor::show3d(ff,8:9,col=fcol(ff),plot_GOF=TRUE,orderByImportance=FALSE)
    #p <- show3d(ff,c(8,9),col=fcol(ff,2),plot_GOF = T)
    #p <- show3d(ff,3:4,col=fcol(ff,2),plot.rgl=list(size=2))
    
    setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")
    
    pdf("test.pdf", width = 7, height = 5)
    p
    dev.off()
    
    library("randomForestExplainer")
    explain_forest(rfo, interactions = FALSE, data = training)
    
    #Distribution of minimal depth
    #To obtain the distribution of minimal depth we pass our forest to the function min_depth_distribution and store the result, which contains the following columns (we save this and load it from memory as it takes a while):
      min_depth_frame <- min_depth_distribution(rfo)
      # save(min_depth_frame, file = "min_depth_frame.rda")
      #load("min_depth_frame.rda")
    head(min_depth_frame, n = 10)
    
    # plot_min_depth_distribution(forest) # gives the same result as below but takes longer
    plot_min_depth_distribution(min_depth_frame)
    plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)
    
     importance_frame <- measure_importance(rfo)
    # save(importance_frame, file = "importance_frame.rda")
    #load("importance_frame.rda")
    importance_frame
    
    #Multi-way importance plot
    #Below we present the result of plot_multi_way_importance for the default values of x_measure and y_measure, which specify measures to use on xx and yy-axis, and the size of points reflects the number of nodes split on the variable. 
    # plot_multi_way_importance(forest, size_measure = "no_of_nodes") # gives the same result as below but takes longer
    plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
    
    #plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)
    
    #Compare measures using ggpairs
    #Generally, the multi-way importance plot offers a wide variety of possibilities so it can be hard to select the most informative one. One idea of overcoming this obstacle is to first explore relations between different importance measures to then select three that least agree with each other and use them in the multi-way importance plot to select top variables. The first is easily done by plotting selected importance measures pairwise against each other using plot_importance_ggpairs as below. One could of course include all seven measures in the plot but by default pp-value and the number of trees are excluded as both carry similar information as the number of nodes.
    # plot_importance_ggpairs(forest) # gives the same result as below but takes longer
    plot_importance_ggpairs(importance_frame)
    
    #Compare different rankings
    #In addition to scatter plots and correlation coefficients, the ggpairs plot also depicts density estimate for each importance measure – all of which are in this case very skewed. An attempt to eliminate this feature by plotting rankings instead of raw measures is implemented in the function plot_importance_rankings that also includes the fitted LOESS curve in each plot.
    # plot_importance_rankings(forest) # gives the same result as below but takes longer
    plot_importance_rankings(importance_frame)
    
    #Variable interactions
    #Conditional minimal depth
    # (vars <- important_variables(forest, k = 5, measures = c("mean_min_depth", "no_of_trees"))) # gives the same result as below but takes longer
    (vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))
    
    interactions_frame <- min_depth_interactions(rfo, vars)
    # save(interactions_frame, file = "interactions_frame.rda")
    #load("interactions_frame.rda")
    head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])
    
    #Then, we pass our interactions_frame to the plotting function plot_min_depth_interactions and obtain the following:
    # plot_min_depth_interactions(forest) # calculates the interactions_frame for default settings so may give different results than the function below depending on our settings and takes more time
    plot_min_depth_interactions(interactions_frame)
    
    interactions_frame <- min_depth_interactions(rfo, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
    # save(interactions_frame, file = "interactions_frame_relevant.rda")
    #load("interactions_frame_relevant.rda")
    plot_min_depth_interactions(interactions_frame)
    
    plot_predict_interaction(rfo, training, "COMPARE_AGE", "LEVENSHTEIN")
    
    library(plotmo)
    plotmo(rfo, type="prob")
    
    library(kohonen)
    
    # Create the SOM Grid - you generally have to specify the size of the 
    # training grid prior to training the SOM. Hexagonal and Circular 
    # topologies are possible
    som_grid <- somgrid(xdim = 15, ydim=15, topo="hexagonal")
    
    # Finally, train the SOM, options for the number of iterations,
    # the learning rates, and the neighbourhood are available
    data.full <- data.full[ , -which(names(data.full) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]
    
    tt <- training
    tt$CLASS <- as.character(tt$CLASS)
    tt[grep("trust", tt$CLASS), "CLASS"] <- 0
    tt[grep("decep", tt$CLASS), "CLASS"] <- 1
    tt$CLASS <- as.numeric(tt$CLASS)
    data_train_matrix <- as.matrix(scale(tt))
    
    s <- par("mar")
    par(mar=c(2,2,2,2))
    som_model <- som(data_train_matrix, 
                     grid=som_grid, 
                     rlen=250, 
                     alpha=c(0.05,0.01), 
                     keep.data = TRUE)
    #n.hood="circular" )
    
    #remove datasets not required further
    rm(som_grid, data_train_matrix)
    
    #Visualise the SOM model results
    # Plot of the training progress - how the node distances have stabilised over time.
    
    plot(som_model, type = "changes")
    
    ## custom palette as per kohonen package (not compulsory)
    coolBlueHotRed <- function(n, alpha = 1) {
      rainbow(n, end=4/6, alpha=alpha)[n:1]
    }
    
    #counts within nodes
    plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
    #map quality
    plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
    
    #neighbour distances
    plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
    
    
    #code spread
    plot(som_model, type = "codes")
    
    # Plot the heatmap for a variable at scaled / normalised values
    var <- 19
    plot(som_model, type = "property", property = som_model$codes[[1]][,var], main=names(testing)[var], palette.name=coolBlueHotRed)
    
    library(grid)
    library(gridBase)
    
    my.plotheatmap <- function(arg1){
      var <- arg1 #define the variable to plot 
      testing <- tt
      var_unscaled <- aggregate(as.numeric(testing[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
      p <- plot(som_model, type = "property", property=var_unscaled, main=names(testing)[var], palette.name=coolBlueHotRed)
    }
    
    tt <- as.data.frame(data_train_matrix)
    for (i in  1:length(names(tt))){
      my.plotheatmap(i)
    }
    
    
    # show the WCSS metric for kmeans for different clustering sizes.
    # Can be used as a "rough" indicator of the ideal number of clusters
    mydata <- som_model$codes[[1]]
    wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
    for (i in 2:25) wss[i] <- sum(kmeans(mydata,
                                         centers=i)$withinss)
    par(mar=c(5.1,4.1,4.1,2.1))
    plot(1:25, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")
    
    # Colour palette definition
    pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
    
    # Form clusters on grid
    ## use hierarchical clustering to cluster the codebook vectors
    som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 2)
    
    # Show the map with different colours for every cluster						  
    plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
    add.cluster.boundaries(som_model, som_cluster)
    
    #show the same plot with the codes instead of just colours
    plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
    add.cluster.boundaries(som_model, som_cluster)
    
    
    
    
    
    