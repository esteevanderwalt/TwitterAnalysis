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
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, FOLLOWERS_COUNT, FRIENDS_COUNT, FF_RATIO, LISTED_COUNT, USERNAME_LENGTH, GEO_ENABLED, PROFILE_HAS_URL, IS_CELEBRITY, ACCOUNT_AGE_IN_MONTHS, LANGUAGE, HAS_NAME, HAS_IMAGE, DUP_PROFILE, HAS_PROFILE, STATUS_COUNT, CLASS from TWITTER.ZZ_BOT_SET") )

close(myconn)

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
data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME","IS_CELEBRITY"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterBot(data.full)

#show histogram of data
data.clean[,"FRIENDS_COUNT"]
data.clean[data.clean$FRIENDS_COUNT>3000,"FRIENDS_COUNT"]

science_themel = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), legend.position = c(0.85,0.7), text = element_text(size = 10))
science_theme = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), text = element_text(size = 10))
science_themell = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), legend.position = c(0.85,0.1), text = element_text(size = 10))

graph1 <- function(data, a, b) {
  p <- ggplot(data, aes(get(a))) + #, fill = CLASS
    theme_bw() +
    science_themel +
    labs(y = "Scaled value", x = a) +
    geom_dotplot(binwidth = b) 
  #print(p)
  
  png(filename = paste("robo_",a,"_v1.png",sep=""), width = 500, height = 250)
  print(p)
  dev.off()
}

graph2 <- function(data, a, b) {
  p <- ggplot(data, aes(get(a))) + #, fill = CLASS
    theme_bw() +
    science_themel +
    labs(y = "Total", x = a) +
    geom_histogram(binwidth = b) 
  #print(p)
  
  png(filename = paste("robo_hist_",a,"_v1.png",sep=""), width = 500, height = 250)
  print(p)
  dev.off()
}

graph1(data.clean,"FRIENDS_COUNT", 50)
graph1(data.clean,"FOLLOWERS_COUNT", 150)
graph1(data.clean,"FF_RATIO", 50)
graph1(data.clean,"LISTED_COUNT", 250)
graph1(data.clean,"USERNAME_LENGTH", 0.5)
graph1(data.clean,"GEO_ENABLED", 0.1)
graph1(data.clean,"PROFILE_HAS_URL", 0.1)
graph1(data.clean,"ACCOUNT_AGE_IN_MONTHS", 1)
graph1(data.clean,"LANGUAGE", 0.1)
graph1(data.clean,"HAS_NAME", 0.1)
graph1(data.clean,"HAS_IMAGE", 0.1)
graph1(data.clean,"DUP_PROFILE", 0.1)
graph1(data.clean,"HAS_PROFILE", 0.1)
graph1(data.clean,"STATUS_COUNT", 150)

graph2(data.clean,"FRIENDS_COUNT", 50)
graph2(data.clean,"FOLLOWERS_COUNT", 150)
graph2(data.clean,"FF_RATIO", 50)
graph2(data.clean,"LISTED_COUNT", 250)
graph2(data.clean,"USERNAME_LENGTH", 0.5)
graph2(data.clean,"GEO_ENABLED", 0.5)
graph2(data.clean,"PROFILE_HAS_URL", 0.5)
graph2(data.clean,"ACCOUNT_AGE_IN_MONTHS", 1)
graph2(data.clean,"LANGUAGE", 1)
graph2(data.clean,"HAS_NAME", 0.5)
graph2(data.clean,"HAS_IMAGE", 0.5)
graph2(data.clean,"DUP_PROFILE", 0.5)
graph2(data.clean,"HAS_PROFILE", 0.5)
graph2(data.clean,"STATUS_COUNT", 50)

