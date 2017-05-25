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
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from TWITTER.ZZ_RFE_SET") )

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
data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterFE(data.full)

#was done in cleanup
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("USERNAME_LENGTH", "GEO_ENABLED"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "DUP_PROFILE", "HAS_PROFILE"))]

#remove unique values
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("LOCATION", "LONGITUDE", "LATITUDE"))]

data.scaled <- as.data.frame(scale(data.clean[1:7], center = TRUE, scale = TRUE))
#add class back
data.scaled <- cbind(data.scaled,CLASS=data.clean[,8])

#show mean per CLASS
#require(dplyr)
#data.scaled %>%
#  group_by(CLASS) %>%
#  summarise_each(funs(max, min, mean, median, sd), FRIENDS_COUNT)

rapply(data.clean,function(x)length(unique(x)))
rapply(data.clean,function(x)sum(is.na(x)))

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/RFE3_After_Chi.txt"
sink(filename, append = TRUE)
chi(data.scaled)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/RFE3_After_Fisher.txt"
sink(filename, append = TRUE)
fisher(data.scaled)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/RFE3_After_Wilcoxon.txt"
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

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/RFE3_After_VarTest.txt"
sink(filename, append = TRUE)
vtest(data.scaled)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/RFE3_AfterAttrImportance.txt"
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
  science_themell +
  labs(x = "CLASS", y = "Metadata") +
  geom_boxplot(notch=FALSE)
print(p)

setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")

png(filename = "rfe3_boxplot.png", width = 500, height = 250)
p
dev.off()

graph1 <- function(data, a) {
  p <- ggplot(data, aes(x=get(a), colour=CLASS, linetype=CLASS)) +
    theme_bw() +
    science_themel +
    labs(y = "Scaled value", x = a) +
    geom_density(size=1.0) 
  #print(p)
  
  png(filename = paste("rfe3_",a,"_v1.png",sep=""), width = 500, height = 250)
  print(p)
  dev.off()
}

graph1(data.scaled,"DISTANCE_LOCATION")
graph1(data.scaled,"DISTANCE_TZ")
graph1(data.scaled,"COMPARE_GENDER")
graph1(data.scaled,"LEVENSHTEIN")
graph1(data.scaled,"HAMMING")
graph1(data.scaled,"COMPARE_AGE")
graph1(data.scaled,"FF_RATIO")
graph1(data.scaled,"PROFILE_HAS_URL")
graph1(data.scaled,"DUP_PROFILE")
graph1(data.scaled,"HAS_PROFILE")
graph1(data.scaled,"LISTED_COUNT")
