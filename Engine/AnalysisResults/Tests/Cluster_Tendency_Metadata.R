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

#LINUX
setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")

source("LIB_Cleanup.R")
source("LIB_ML_Models_ROC.R")
source("LIB_Stats.R")

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, CLASS from twitter.zz_full_set") )
close(myconn)

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

data.full <- data.full[ , -which(names(data.full) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.Twitter(data.full)

#remove NZV values
#was done in cleanup
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("BACKGROUND_IMAGE", "IS_BACKGROUND_IMAGE_USED", "PROFILE_TEXT_COLOR", "PROFILE_BG_COLOR"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("CREATED", "ORIGINAL_PROFILE_IMAGE"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("GEO_ENABLED", "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE"))]

#perform hopkins stat test for cluster tendency
data.o <- data.clean

data.o <- data.o[ , -which(names(data.o) %in% c("LOCATION", "LONGITUDE", "LATITUDE"))]
data.o <- data.o[ , -which(names(data.o) %in% c("LANGUAGE","UTC_OFFSET", "PROFILE_IMAGE"))]
#data.o <- data.o[ , -which(names(data.o) %in% c("CLASS"))]

inTrain <- createDataPartition(y = data.o$CLASS, p = .99, list = FALSE)

training <- data.o[inTrain,]
testing <- data.o[-inTrain,]
#rm(inTrain) 

#cl <- makeCluster(detectCores())
#registerDoParallel(cores=7) #or cl
#head(iris)
#df = scale(iris[, -5])
df = scale(testing[,-6])

pca <- prcomp(df, repel = TRUE)

#visual check
fviz_pca_ind(pca, label="none", habillage=testing$CLASS,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2")

#visual check
fviz_pca_ind(pca, title = "Test", 
             habillage = testing$CLASS,  palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

#according to contribution
fviz_pca_ind(pca, col.ind="contrib")
fviz_pca_ind(pca, col.ind="contrib") + scale_color_gradient2(low="blue", mid="white",high="red", midpoint=4)
fviz_pca_ind(pca, col.ind="contrib") + scale_color_gradient2(low="blue", mid="white", high="red", midpoint=4) + theme_minimal()

# Example: Select the top 40 according to the cos2
fviz_pca_ind(pca, select.ind = list(cos2 = 40))

# Graph of variables
# ++++++++++++++++++++++++++++
# Default plot
fviz_pca_var(pca, col.var = "steelblue")

# Control variable colors using their contributions
fviz_pca_var(pca, col.var = "contrib", 
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

# Biplot of individuals and variables
# ++++++++++++++++++++++++++
# Keep only the labels for variables
# Change the color by groups, add ellipses
fviz_pca_biplot(pca, label = "var", habillage=testing$CLASS,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())

#get optimal number of clusters
fviz_nbclust(df, kmeans, method = "wss")

#Variable contributions to the principal axes:
var <- get_pca_var(pca)
head(var$contrib)
# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 10)

set.seed(123)
# K-means on iris dataset
km.res1 <- kmeans(df, 2)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

library(factoextra)
# Compute Hopkins statistic for iris dataset
t2 <- system.time(res <- get_clust_tendency(df, n = nrow(df)-1, graph = FALSE))
#t2 <- system.time(res <- get_clust_tendency(data.o, n = 50, graph = FALSE))
res$hopkins_stat

#The algorithm of the visual assessment of cluster tendency (VAT) approach (Bezdek and Hathaway, 2002) 
# http://www.sthda.com/english/articles/29-cluster-validation-essentials/95-assessing-clustering-tendency-essentials/
fviz_dist(dist(df), show_labels = FALSE)+
  labs(title = "Social media data")

#stopCluster(cl)
