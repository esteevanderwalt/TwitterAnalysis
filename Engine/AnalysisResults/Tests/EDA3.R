suppressMessages(library(dplyr))
suppressMessages(library(RODBC))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(doParallel))
suppressMessages(library(FSelector))

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

#first get nzv values
nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
nzv[nzv$nzv,]

rapply(data.full,function(x)length(unique(x)))
rapply(data.original,function(x)sum(is.na(x)))
#all orig
i1 <- data.original[data.original$CLASS=='deceptive',]
rapply(i1,function(x)length(unique(x)))
#all injected
i2 <- data.original[data.original$CLASS=='trustworthy',]
rapply(i2,function(x)length(unique(x)))


data.full <- data.full[ , -which(names(data.full) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/Before_Chi.txt"
sink(filename, append = TRUE)
chi(data.full)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/Before_Wilcoxon.txt"
sink(filename, append = TRUE)
wilcoxon.freq(data.full)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/BeforeAttrImportance.txt"
sink(filename, append = TRUE)
cat("\n")
print("Attribute importance - Corpus")
print("=================================")
imp(data.full)
sink()

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

#get bin plots

t <- data.clean[data.clean$FOLLOWERS_COUNT < 100,]

p <- ggplot(t, aes(x=FOLLOWERS_COUNT)) + 
  geom_bar(stat="count") + 
  theme_bw() +
  science_theme +
  #scale_x_continuous(name="# of friends", label=comma) +
  scale_y_continuous(name="# of accounts per followers_count group", label=comma) +
  labs(x = "Bin #") 
print(p)

setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")
pdf("binning_followers.pdf", width = 7, height = 5)
p
dev.off()

M <- cor(data.clean[,1:11])

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(data.clean[,1:11])

library(scales)
library(corrplot)

corrplot(M, method="number")
# alternative to use a corrplot
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.00005,
         tl.col="black")
print(p)

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

mosthighlycorrelated(data.clean[,1:11], 10)

nzv <- nearZeroVar(data.clean, saveMetrics= TRUE)
nzv[nzv$nzv,]

#remove unique values
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("LOCATION", "LONGITUDE", "LATITUDE"))]

data.scaled <- as.data.frame(scale(data.clean[1:11], center = TRUE, scale = TRUE))
#add class back
data.scaled <- cbind(data.scaled,CLASS=data.clean[,12])

#show mean per CLASS
require(dplyr)
data.scaled %>%
  group_by(CLASS) %>%
  summarise_each(funs(max, min, mean, median, sd), FRIENDS_COUNT)

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/After_Chi.txt"
sink(filename, append = TRUE)
chi(data.scaled)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/After_Fisher.txt"
sink(filename, append = TRUE)
fisher(data.scaled)
sink()
source("LIB_Stats.R")

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/After_Wilcoxon.txt"
sink(filename, append = TRUE)
wilcoxon(data.scaled)
sink()

#options(max.print=100)
#getOption("max.print")

data <- data.clean
mytable <- table(data$TIMEZONE,data$CLASS)
#filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/test.txt"
#sink(filename, append = TRUE)
mytable
if(nrow(mytable)>10) {
  x<-10 
} else {
  x<-nrow(mytable)
}
mytable[1:x,]

#sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/After_VarTest.txt"
sink(filename, append = TRUE)
vtest(data.scaled)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/AfterAttrImportance.txt"
sink(filename, append = TRUE)
cat("\n")
print("Attribute importance - Corpus")
print("=================================")
imp(data.scaled)
sink()


science_themel = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), legend.position = c(0.85,0.7), text = element_text(size = 10))
science_theme = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), text = element_text(size = 10))
science_themell = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), legend.position = c(0.85,0.1), text = element_text(size = 10))

library(ggplot2)
library(reshape2)

#box
d <- melt(data.scaled)
# Basic box plot
p <- ggplot(d, aes(x=CLASS, y=value, fill=CLASS)) + 
  facet_wrap(~variable,scales = "free_x") +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  science_theme +
  labs(x = "Class", y = "Metadata") +
  geom_boxplot(notch=FALSE)
print(p)

setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")

pdf("metadata_distribution.pdf", width = 7, height = 6)
p
dev.off()

graph1 <- function(data, a) {
  p <- ggplot(data, aes(x=get(a), colour=CLASS, linetype=CLASS)) +
    theme_bw() +
    science_themel +
    labs(y = "Scaled value", x = a) +
    geom_density(size=1.0) 
  #print(p)
  
  pdf(paste("meta_",a,"_v2.png",sep=""), width = 6, height = 4)
  print(p)
  dev.off()
}

graph1(data.scaled,"PROFILE_IMAGE")
graph1(data.scaled,"STATUS_COUNT")
graph1(data.scaled,"UTC_OFFSET")
graph1(data.scaled,"TIMEZONE")
graph1(data.scaled,"LISTED_COUNT")
graph1(data.scaled,"LANGUAGE")
graph1(data.scaled,"FRIENDS_COUNT")
graph1(data.scaled,"FOLLOWERS_COUNT")

