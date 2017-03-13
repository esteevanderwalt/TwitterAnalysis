# Load packages
library(caret)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "twitter", host = "localhost", port = 5432, user = "postgres", password = "postgres")

#' ###Load data
#+ get_data
data.original <- dbGetQuery(con, "SELECT * from main.zz_full_set") 
data.full <- data.original

#get information gain for all parameters
#library(FSelector)
#weights <- linear.correlation(class~., data.full)
#print(weights)
#subset <- cutoff.k(weights, 3)
#f <- as.simple.formula(subset, "class")
#print(f)

#weights <- information.gain(class~., data.full)
#print(weights)
#weights <- gain.ratio(class~., data.full)

#subset <- cutoff.k(weights, 2)
#f <- as.simple.formula(subset, "class")
#print(f)

#weights <- random.forest.importance(class~., data.full, importance.type = 1)
#print(weights)
#subset <- cutoff.k(weights, 5)
#f <- as.simple.formula(subset, "class")
#print(f)

#' ######################################
#' ###  Cleanup and preprocessing
#' ######################################
#+ clean_preprocess
data.full$sentiment[is.na(data.full$sentiment)] <- 'Other'
data.full$emotion[is.na(data.full$emotion)] <- 'Other'
data.full$distance_location[is.na(data.full$distance_location)] <- 0
data.full$distance_tz[is.na(data.full$distance_tz)] <- 0
data.full$continent[is.na(data.full$continent)] <- 'Other'
data.full$sub_region[is.na(data.full$sub_region)] <- 'Other'
data.full$gender[is.na(data.full$gender)] <- 'Other'
data.full$gender[data.full$gender=='1M'] <- 'M'
data.full$gender[data.full$gender=='?M'] <- 'M'
data.full$gender[data.full$gender=='1F'] <- 'F'
data.full$gender[data.full$gender=='?F'] <- 'F'
data.full$avg_tweet_time[is.na(data.full$avg_tweet_time)] <- 12
data.full$no_of_devices[is.na(data.full$no_of_devices)] <- 1
data.full$levenshtein[is.na(data.full$levenshtein)] <- 1
data.full$hamming[is.na(data.full$hamming)] <- 1
data.full$valid_name[is.na(data.full$valid_name)] <- 0
data.full$image_gender[is.na(data.full$image_gender)] <- 'Other'
data.full$image_age[is.na(data.full$image_age)] <- 20
data.full$no_of_faces[is.na(data.full$no_of_faces)] <- 20
#change last tweet time
data.full$last_tweet_time <- year(ymd_hms(data.full$last_tweet_time))
data.full$last_tweet_time[is.na(data.full$last_tweet_time)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(continent) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(continent %in% d$continent[1:50]))$continent
data.full$continent[data.full$continent %in% l] <- 'Other'
rm(d, l)
#remove decimals from numerics
data.full$image_age <- round(data.full$image_age)
data.full$avg_tweet_time <- round(data.full$avg_tweet_time)
#update name
data.full[data.full$valid_name != 0,]$valid_name <- 1
#first replace NA with other
data.full$timezone[is.na(data.full$timezone)] <- 'Other'
data.full$latitude[is.na(data.full$latitude)] <- 0
data.full$longitude[is.na(data.full$longitude)] <- 0
#change created to be year of creation
data.full$created <- year(ymd_hms(data.full$created))
data.full$created[is.na(data.full$created)] <- 2000
#change location, language, timezone to only have top50 and other
d <- data.full %>% 
  group_by(location) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(location %in% d$location[1:50]))$location
data.full$location[data.full$location %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(timezone) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(timezone %in% d$timezone[1:20]))$timezone
data.full$timezone[data.full$timezone %in% l] <- 'Other'
rm(d, l)
d <- data.full %>% 
  group_by(language) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
l <- subset(data.full, !(language %in% d$language[1:20]))$language
data.full$language[data.full$language %in% l] <- 'Other'
rm(d, l)
#remove decimals from lat/lon
data.full$latitude <- round(data.full$latitude)
data.full$longitude <- round(data.full$longitude)

#' ######################################
#' ### Prepare Datasets with dummy vars
#' ######################################
#+ prepare
myvars <- c("utc_offset",
            "geo_enabled", "latitude", "longitude",  
            "is_default_profile", "is_default_profile_image", "class")
setwd("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults")
source("ML_libs.R")
source("run_ML.R")
data.o <- prepareData(data.full[myvars])

#' ######################################
#' ### Run model and print results
#' ######################################
#+ run_models
set.seed(123)

inTrain <- createDataPartition(y = data.o$class, p = .75, list = FALSE)
#str(inTrain)

training <- data.o[inTrain,]
testing <- data.o[-inTrain,]
rm(inTrain)  

filename <- "Results/7attr_npROC_cv_5fold_0repeat_3tune.txt"
folds <- 5
repeats <- 0
resamp <- "cv"
tune <- 3

t <- system.time(run_ML(training, resamp, folds, tune, repeats, filename))

sink(filename, append = TRUE)

cat("\n")
print("Total run time")
print("==============")
print(t)

sink()