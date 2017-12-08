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
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from TWITTER.ZZ_FE_SET") )

close(myconn)

#remove NZV values
data.full <- data.original
data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
data.clean <- cleanup.TwitterFE(data.full)

#remove accounts with 0
proof <- function(data, c){
  attach(data)
  t <- data[ which(get(c) != 0),]
  detach(data)

  #get bin plots
  p <- ggplot(t, aes(x=get(c), fill=CLASS)) + 
    geom_bar(stat="count") + 
    theme_bw() +
    science_theme +
    #scale_x_continuous(name="# of friends", label=comma) +
    scale_y_continuous(name="# of accounts per followers_count group", label=comma) +
    labs(x = "Bin #") 
  print(p)
}

proof(data.clean, "DISTANCE_LOCATION")
proof(data.clean, "DISTANCE_TZ")
proof(data.clean, "LEVENSHTEIN")
proof(data.clean, "HAMMING")
proof(data.clean, "COMPARE_GENDER")
proof(data.clean, "COMPARE_AGE")

proof(data.clean, "DISTANCE_LOCATION")
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
  labs(x = "Class", y = "SMP Attributes") +
  geom_boxplot(notch=FALSE)
print(p)

graph1 <- function(data, a) {
  p <- ggplot(data, aes(x=get(a), colour=CLASS, linetype=CLASS)) +
    theme_bw() +
    science_themel +
    labs(y = "Data value", x = a) +
    geom_density(size=1.0) 
  #print(p)
  
  #pdf(paste("meta_",a,"_v2.png",sep=""), width = 6, height = 4)
  print(p)
  #dev.off()
}

graph1(data.clean,"PROFILE_IMAGE")
graph1(data.clean,"STATUS_COUNT")
graph1(data.clean,"UTC_OFFSET")
graph1(data.clean,"TIMEZONE")
graph1(data.clean,"LISTED_COUNT")
graph1(data.clean,"LANGUAGE")
graph1(data.clean,"FRIENDS_COUNT")
graph1(data.clean,"FOLLOWERS_COUNT")
