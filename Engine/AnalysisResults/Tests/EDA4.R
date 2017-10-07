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
#tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, CLASS from twitter.zz_full_set") )
#bot
#tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, FOLLOWERS_COUNT, FRIENDS_COUNT, FF_RATIO, LISTED_COUNT, USERNAME_LENGTH, GEO_ENABLED, PROFILE_HAS_URL, IS_CELEBRITY, ACCOUNT_AGE_IN_MONTHS, LANGUAGE, HAS_NAME, HAS_IMAGE, DUP_PROFILE, HAS_PROFILE, STATUS_COUNT, CLASS from TWITTER.ZZ_BOT_SET") )
#fe
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, SCREENNAME, DISTANCE_LOCATION, DISTANCE_TZ, COMPARE_GENDER, LEVENSHTEIN, HAMMING, COMPARE_AGE, FF_RATIO, PROFILE_HAS_URL, DUP_PROFILE, HAS_PROFILE, LISTED_COUNT, CLASS from TWITTER.ZZ_FE_SET") )

close(myconn)

#dim(data.original)
#save(data.original,file="data.original.RData")
#load("data.original.RData")
data.full <- data.original

rapply(data.full,function(x)length(unique(x)))
rapply(data.original,function(x)sum(is.na(x)))
#all orig
i1 <- data.original[data.original$CLASS=='deceptive',]
rapply(i1,function(x)length(unique(x)))
#all injected
i2 <- data.original[data.original$CLASS=='trustworthy',]
rapply(i2,function(x)length(unique(x)))


#data.full <- data.full[ , -which(names(data.full) %in% c("ID","NAME","SCREENNAME","DESCRIPTION","IS_CELEBRITY","LAST_TWEET"))]
#bot
data.full <- data.full[ , -which(names(data.full) %in% c("ID","SCREENNAME","IS_CELEBRITY"))]

#change factors to characters
data.full <- cleanup.factors(data.full)
#clean
#data.clean <- cleanup.Twitter(data.full)
#bot
#data.clean <- cleanup.TwitterBot(data.full)
#fe
data.clean <- cleanup.TwitterFE(data.full)

#remove fields not in fake accounts
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("BACKGROUND_IMAGE", "IS_BACKGROUND_IMAGE_USED", "PROFILE_TEXT_COLOR", "PROFILE_BG_COLOR"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("CREATED", "ORIGINAL_PROFILE_IMAGE"))]
#data.clean <- data.clean[ , -which(names(data.clean) %in% c("GEO_ENABLED", "IS_DEFAULT_PROFILE", "IS_DEFAULT_PROFILE_IMAGE"))]

#fe
data.clean <- data.clean[ , -which(names(data.clean) %in% c("FF_RATIO","PROFILE_HAS_URL", "DUP_PROFILE", "HAS_PROFILE", "LISTED_COUNT"))]
data.clean <- data.clean[ , -which(names(data.clean) %in% c("HAMMING"))]

rapply(data.clean,function(x)length(unique(x)))
#all orig
i1 <- data.clean[data.clean$CLASS=='deceptive',]
rapply(i1,function(x)length(unique(x)))
#all injected
i2 <- data.clean[data.clean$CLASS=='trustworthy',]
rapply(i2,function(x)length(unique(x)))


data.scaled <- as.data.frame(scale(data.clean[1:5], center = TRUE, scale = TRUE))
#add class back
data.scaled <- cbind(data.scaled,CLASS=data.clean[,6])

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
  labs(x = "Class", y = "Engineered features") +
  geom_boxplot(notch=FALSE)
print(p)

setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")

pdf("metadata_postprep_distribution.pdf", width = 7, height = 5)
p
dev.off()
