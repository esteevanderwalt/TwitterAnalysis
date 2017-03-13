#' ---
#' title: Using SVM - Example
#' author: 
#' date: 
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#'      keep_md: yes
#' ---

#+ setup, include=FALSE
suppressMessages(library(knitr))
suppressMessages(library(ggplot2))
suppressMessages(library(scales))
suppressMessages(library(caret))
suppressMessages(library(lubridate))
suppressMessages(library(doParallel))
suppressMessages(library(pROC))
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(RPostgreSQL))

#load external script file
setwd("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults")
source("ML_libs.R")
if(!exists("ggplot_missing", mode="function")) source("ML_libs.R")

#Connect to the database and pull data
#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
#creates a connection to the postgres database
#note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "twitter", host = "localhost", port = 5432, user = "postgres", password = "")

#' ###Load data
#+ get_data
data.original <- dbGetQuery(con, "SELECT * from main.zz_full_set") 
data.full <- data.original

#' ###Plot missing data
#+ plot, fig.width=15, fig.height=15
p <- ggplot_missing(data.full)
print(p)

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
            "is_default_profile", "is_default_profile_image", "created", "class")
data.o <- prepareData(data.full[myvars])

myvars <- c("distance_location","distance_tz",
            "gender","levenshtein","hamming","valid_name","image_gender","image_age",
            "no_of_faces", "class")
data.e <- prepareData(data.full[myvars])

#' ######################################
#' ### Run model and print results
#' ######################################
#+ run_models
#data = full set with class
#dummy = dummyVars set without class
svmFit.o <- runModel(data.o, "original","svmRadial", 8, 0)
svmFit.e <- runModel(data.e, "engineer","svmRadial", 8, 0)

svmFit.o <- train(class ~ ., data = data.o, method = "svmRadial")

#' ######################################
#' ### Compare models
#' ######################################
#+ compare_models
resampling(list(opls = svmFit.o, epls = svmFit.e))

#svmFit.o <- runModel(data.o, "original","svmRadial", 0)
#svmFit.e <- runModel(data.e, "engineer","svmRadial", 0)
#resampling(list(osvm = svmFit.o, esvm = svmFit.e))
