suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(RODBC))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(doParallel))
suppressMessages(library(FSelector))
suppressMessages(library(pROC))

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")
source("LIB_Stats.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, paste("SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from TWITTER.ZZ_FE_SET", sep="") ) )

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

#first get nzv values
#nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
#nzv[nzv$nzv,]

#rapply(data.full,function(x)length(unique(x)))
#rapply(d,function(x)sum(is.na(x)))

#remove NZV values
data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterFE(data.full)

#was done in cleanup
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("FF_RATIO","PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("USERNAME_LENGTH", "GEO_ENABLED"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "DUP_PROFILE", "HAS_PROFILE"))]

#perform machine learning
data.o <- data.clean

#' ######################################
#' ### Run model and print results
#' ######################################
#+ run_models
set.seed(123)

testing <- data.o

close(myconn)

################################
## Run various params on dataset
################################

library(kohonen)

# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 

testing$CLASS <- as.character(testing$CLASS)
testing[grep("trust", testing$CLASS), "CLASS"] <- 0
testing[grep("decep", testing$CLASS), "CLASS"] <- 1
testing$CLASS <- as.numeric(testing$CLASS)
data_train_matrix <- as.matrix(scale(testing))

# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
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
var <- 5
plot(som_model, type = "property", property = som_model$codes[[1]][,var], main=names(testing)[var], palette.name=coolBlueHotRed)

library(grid)
library(gridBase)

my.plotheatmap <- function(arg1){
  var <- arg1 #define the variable to plot 
  var_unscaled <- aggregate(as.numeric(testing[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
  p <- plot(som_model, type = "property", property=var_unscaled, main=names(testing)[var], palette.name=coolBlueHotRed)
}

for (i in  1:length(names(testing))){
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
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 7)

# Show the map with different colours for every cluster						  
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)


