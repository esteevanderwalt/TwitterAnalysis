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
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, FF_RATIO, USERNAME_LENGTH, PROFILE_HAS_URL, ACCOUNT_AGE_IN_MONTHS, HAS_NAME, HAS_IMAGE, DUP_PROFILE, HAS_PROFILE, CLASS from twitter.zz_together_set") )

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
data.clean <- cleanup.TwitterBot(data.clean)

#remove NZV values
#was done in cleanup
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]
#remove fields not in fake accounts
data.clean <- data.clean[ , -which(names(data.clean) %in% c("BACKGROUND_IMAGE", "IS_BACKGROUND_IMAGE_USED", "PROFILE_TEXT_COLOR", "PROFILE_BG_COLOR"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("CREATED", "ORIGINAL_PROFILE_IMAGE"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("GEO_ENABLED", "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE"))]

#remove unique values
data.clean <- data.clean[ , -which(names(data.clean) %in% c("LOCATION", "LONGITUDE", "LATITUDE"))]

#scaled down version
data.clean <- data.clean[ , -which(names(data.clean) %in% c("ID", "NAME", "SCREENNAME", "CREATED", "ORIGINAL_PROFILE_IMAGE", "BACKGROUND_IMAGE", "LAST_TWEET", "DESCRIPTION", "LOCATION", "FRIENDS_COUNT", "STATUS_COUNT", "LISTED_COUNT", "TIMEZONE", "LATITUDE", "LONGITUDE", "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE", "IS_BACKGROUND_IMAGE_USED", "PROFILE_TEXT_COLOR", "PROFILE_BG_COLOR", "FF_RATIO", "DUP_PROFILE", "PROFILE_HAS_URL", "ACCOUNT_AGE_IN_MONTHS", "HAS_NAME", "HAS_IMAGE", "HAS_PROFILE"))]

data.scaled <- as.data.frame(scale(data.clean[1:6], center = TRUE, scale = TRUE))
#add class back
data.scaled <- cbind(data.scaled,CLASS=data.clean[,7])


#show mean per CLASS
require(dplyr)
data.scaled %>%
  group_by(CLASS) %>%
  summarise_each(funs(max, min, mean, median, sd), FRIENDS_COUNT)

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/TAfter_Chi.txt"
sink(filename, append = TRUE)
chi(data.scaled)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/TAfter_Fisher.txt"
sink(filename, append = TRUE)
fisher(data.scaled)
sink()
source("LIB_Stats.R")

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/TAfter_Wilcoxon.txt"
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

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/TAfter_VarTest.txt"
sink(filename, append = TRUE)
vtest(data.scaled)
sink()

filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/TAfterAttrImportance.txt"
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
  labs(x = "CLASS", y = "Metadata") +
  geom_boxplot(notch=FALSE)
print(p)

setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")

png(filename = "together_less2_distribution.png", width = 500, height = 250)
p
dev.off()

graph1 <- function(data, a) {
  p <- ggplot(data, aes(x=get(a), colour=CLASS, linetype=CLASS)) +
    theme_bw() +
    science_themel +
    labs(y = "Scaled value", x = a) +
    geom_density(size=1.0) 
  #print(p)
  
  png(filename = paste("together_",a,"_v1.png",sep=""), width = 500, height = 250)
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
graph1(data.scaled,"FF_RATIO")
graph1(data.scaled,"USERNAME_LENGTH")
graph1(data.scaled,"PROFILE_HAS_URL")
graph1(data.scaled,"ACCOUNT_AGE_IN_MONTHS")
graph1(data.scaled,"HAS_NAME")
graph1(data.scaled,"HAS_IMAGE")
graph1(data.scaled,"DUP_PROFILE")
graph1(data.scaled,"HAS_PROFILE")

