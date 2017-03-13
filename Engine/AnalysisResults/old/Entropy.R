#' ---
#' title: Entropy
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
source("Entropy_libs.R")
source("ML_libs.R")
if(!exists("entropy", mode="function")) source("Entropy_libs.R")
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
#' ### Determine IG for original data
#' ######################################
#+ IG_original
df <- NULL
b <- 5
df <- rbind(df, data.frame(Attribute="utc_offset", IG=IG_numeric(data.full, "utc_offset", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="geo_enabled", IG=IG_numeric(data.full, "geo_enabled", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="latitude", IG=IG_numeric(data.full, "latitude", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="longitude", IG=IG_numeric(data.full, "longitude", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="is_default_profile", IG=IG_numeric(data.full, "is_default_profile", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="is_default_profile_image", IG=IG_numeric(data.full, "is_default_profile_image", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="created", IG=IG_numeric(data.full, "created", "class", bins=b)))

print(df)

#IG_cat(data,feature,target)


#' ######################################
#' ### Determine IG for engineered data
#' ######################################
#+ IG_engineered
df <- NULL
b <- 5
df <- rbind(df, data.frame(Attribute="distance_location", IG=IG_numeric(data.full, "distance_location", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="distance_tz", IG=IG_numeric(data.full, "distance_tz", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="gender", IG=IG_cat(data.full, "gender", "class")))
df <- rbind(df, data.frame(Attribute="levenshtein", IG=IG_numeric(data.full, "levenshtein", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="hamming", IG=IG_numeric(data.full, "hamming", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="valid_name", IG=IG_cat(data.full, "valid_name", "class")))
df <- rbind(df, data.frame(Attribute="image_gender", IG=IG_cat(data.full, "image_gender", "class")))
df <- rbind(df, data.frame(Attribute="image_age", IG=IG_numeric(data.full, "image_age", "class", bins=b)))
df <- rbind(df, data.frame(Attribute="no_of_faces", IG=IG_numeric(data.full, "no_of_faces", "class", bins=b)))

print(df)
