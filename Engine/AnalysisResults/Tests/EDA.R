suppressMessages(library(RODBC))
suppressMessages(library(FSelector))
suppressMessages(library(caret))

#only suppose to run on Linux
z <- "LNX"

if(z=="WIN"){
  #WIN
  filename <- "C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/B_WIN_EDA.txt"
}else{  
  #LINUX
  filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/C_LNX_EDA.txt"
}  

#### connect to DB
if(z=="WIN"){
  #WIN
  myconn<-odbcConnect("FSOC", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
}else{  
  #LINUX
  myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
}  

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_CELEBRITY, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR from twitter.zz_full_set") )
data.full <- data.original

#load external libraries
if(z=="WIN"){
  #WIN
  setwd("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")
}else{  
  #LINUX
  setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")
} 
source("LIB_Cleanup.R")

print("List all attributes available")
print("=============================") 
colnames(data.full)

cat("\n")
print("Amount of data in the dataset")
print("=============================") 
nrow(data.full)

#change factors to characters in dataframe
data.full <- cleanup.factors(data.full)

cat("\n")
print("Data structure")
print("==============") 
str(data.full)
#summary(data.full)

cat("\n")
print("Get unique value count in set")
print("=============================")      
rapply(data.full,function(x)length(unique(x)))

cat("\n")
print("Get null values in set")
print("========================")      
rapply(data.full,function(x)sum(is.na(x)))

#Convert all null values to None
data.full <- cleanup.nulls(data.full)

#Confirm no null values anymore after cleanup
print("Confirm cleanup of nulls")
rapply(data.full,function(x)sum(is.na(x)))

cat("\n")
print("Non/Near zero variance")
print("=======================")      
nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
print(nzv[nzv$nzv,])

#handle nzv
LATITUDE
LONGITUDE
IS_DEFAULT_PROFILE_IMAGE
PROFILE_TEXT_COLOR

#handle characters (convert to numbers / bin)



cat("\n")
print("Correlation")
print("=======================")
#only get numeric fields for correlation
i <- sapply(data.full, is.numeric)
descrCor <-  cor(data.full[i])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .7) #more than 70% correlation

cat("\n")
print("Entropy, IG, IG Ratio")
print("=======================")
#add class column


entropy <- function(x,b) {
  #for each column in x
  dfu <- NULL
  for (j in names(x)) {
    print(j)
    if(j != "CLASS"){
      if(is.numeric(x[,j])){
        print(IG_numeric(x, j, "CLASS", bins=b))
        dfu <- rbind(dfu, data.frame(Attribute=j, IG=IG_numeric(x, j, "CLASS", bins=b)))
      }else{
        #print("no")
        dfu <- rbind(dfu, data.frame(Attribute=j, IG=IG_cat(x, j, "CLASS")))
      }
    }
  }
  return(dfu)
}

df <- entropy(data.full,5)


ggplot(aes(y = FRIENDS_COUNT, x = CLASS), data = data.full) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) 

geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE) + 
  

#' ######################################
#' ###  Cleanup and preprocessing
#' ######################################
#+ clean_preprocess

data.full$GENDER[data.full$GENDER=='1M'] <- 'M'
data.full$GENDER[data.full$GENDER=='?M'] <- 'M'
data.full$GENDER[data.full$GENDER=='1F'] <- 'F'
data.full$GENDER[data.full$GENDER=='?F'] <- 'F'
data.full$AVG_TWEET_TIME[is.na(data.full$AVG_TWEET_TIME)] <- 12
data.full$NO_OF_DEVICES[is.na(data.full$NO_OF_DEVICES)] <- 1
data.full$LEVENSHTEIN[is.na(data.full$LEVENSHTEIN)] <- 1
data.full$HAMMING[is.na(data.full$HAMMING)] <- 1
data.full$VALID_NAME[is.na(data.full$VALID_NAME)] <- 0
data.full$IMAGE_GENDER[is.na(data.full$IMAGE_GENDER)] <- 'Other'
data.full$IMAGE_AGE[is.na(data.full$IMAGE_AGE)] <- 20
data.full$NO_OF_FACES[is.na(data.full$NO_OF_FACES)] <- 20
#change last tweet time
data.full$LAST_TWEET_TIME <- year(ymd_hms(data.full$LAST_TWEET_TIME))
data.full$LAST_TWEET_TIME[is.na(data.full$LAST_TWEET_TIME)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(CONTINENT) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(CONTINENT %in% d$CONTINENT[1:50]))$CONTINENT
data.full$CONTINENT[data.full$CONTINENT %in% l] <- 'Other'
rm(d, l)
#remove decimals from numerics
data.full$IMAGE_AGE <- round(data.full$IMAGE_AGE)
data.full$AVG_TWEET_TIME <- round(data.full$AVG_TWEET_TIME)
#update name
data.full[data.full$VALID_NAME != 0,]$VALID_NAME <- 1
#first replace NA with other
data.full$TIMEZONE[is.na(data.full$TIMEZONE)] <- 'Other'
data.full$LATITUDE[is.na(data.full$LATITUDE)] <- 0
data.full$LONGITUDE[is.na(data.full$LONGITUDE)] <- 0
#change created to be year of creation
data.full$CREATED <- year(ymd_hms(data.full$CREATED))
data.full$CREATED[is.na(data.full$CREATED)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(LOCATION) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(LOCATION %in% d$LOCATION[1:50]))$LOCATION
data.full$LOCATION[data.full$LOCATION %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(TIMEZONE) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(TIMEZONE %in% d$TIMEZONE[1:20]))$TIMEZONE
data.full$TIMEZONE[data.full$TIMEZONE %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(LANGUAGE) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(LANGUAGE %in% d$LANGUAGE[1:20]))$LANGUAGE
data.full$LANGUAGE[data.full$LANGUAGE %in% l] <- 'Other'
rm(d, l)
#remove decimals from lat/lon
data.full$LATITUDE <- round(data.full$LATITUDE)
data.full$LONGITUDE <- round(data.full$LONGITUDE)



sink(filename, append = TRUE)

cat("\n")
print("Query loading run time")
print("==============")
print(tl)

sink()