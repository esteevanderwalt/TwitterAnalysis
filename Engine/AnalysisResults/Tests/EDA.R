suppressMessages(library(RODBC))
suppressMessages(library(FSelector))
suppressMessages(library(entropy))

z <- "WIN"

if(z=="WIN"){
  #WIN
  filename <- "C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/A_WIN_NP_5fold_0repeat_3tune.txt"
}else{  
  #LINUX
  filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/A_LNX_NP_5fold_0repeat_3tune.txt"
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
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT * from twitter.zz_full_set_win") )
data.full <- data.original

#change factors to characters
i <- sapply(data.full, is.factor)
data.full[i] <- lapply(data.full[i], as.character)

#str(data.full)
#summary(data.full)

print("List all attributes available")
print("=============================") 
colnames(data.original)

cat("\n")
print("Amount of data in the dataset")
print("=============================") 
count(data.original)

cat("\n")
print("Get unique values in set")
print("========================")      
rapply(data.original,function(x)length(unique(x)))

cat("\n")
print("Non/Near zero variance")
print("=======================")      
nzv <- nearZeroVar(data.original, saveMetrics= TRUE)
print(nzv[nzv$nzv,])

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
entropy(data.full[i])
IG <- information.gain(CLASS~., data.full)
IGR <- gain.ratio(CLASS~., data.full)


#' ######################################
#' ###  Cleanup and preprocessing
#' ######################################
#+ clean_preprocess

data.full$SENTIMENT[is.na(data.full$SENTIMENT)] <- 'Other'
data.full$EMOTION[is.na(data.full$EMOTION)] <- 'Other'
data.full$DISTANCE_LOCATION[is.na(data.full$DISTANCE_LOCATION)] <- 0
data.full$DISTANCE_TZ[is.na(data.full$DISTANCE_TZ)] <- 0
data.full$UTC_OFFSET[is.na(data.full$UTC_OFFSET)] <- 0
data.full$IS_DEFAULT_PROFILE_IMAGE[is.na(data.full$IS_DEFAULT_PROFILE_IMAGE)] <- 0
data.full$CONTINENT[is.na(data.full$CONTINENT)] <- 'Other'
data.full$SUB_REGION[is.na(data.full$SUB_REGION)] <- 'Other'
data.full$GENDER[is.na(data.full$GENDER)] <- 'Other'
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