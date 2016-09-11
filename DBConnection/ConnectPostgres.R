#install.packages("RPostgreSQL")
# require("RPostgreSQL")

## @knitr connectDB

library(RPostgreSQL)

#create a connection
#save the password that we can "hide" it as best as we can by collapsing it
pw <- {  "" }

#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
#creates a connection to the postgres database
#note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "twitter",
host = "localhost", port = 5432,
user = "postgres", password = "")
#user = postgres for UBUNTU

rm(pw) # removes the password

#Connection success: 
dbExistsTable(con, c("main","experiment_tweets_shortest"))

## @knitr tweets_single_user
tweets.singleuser <- dbGetQuery(con, "SELECT * from main.experiment_tweets_shortest where \"USERNAME\" = 'Londs_'")

## @knitr tweets
tweets <- dbGetQuery(con, "SELECT \"ID\", \"CREATEDAT\", \"USERID\", \"RETWEET\", \"GEO_ENABLED\", \"LATITUDE\", \"LONGITUDE\", \"LOCATION\", \"TIMEZONE\" from main.experiment_tweets_shortest")

## @knitr tweets_content
tweets <- dbGetQuery(con, "SELECT \"USERNAME\", \"USERID\", \"CREATEDAT\", \"CONTENT\",\"GEO_ENABLED\", \"LATITUDE\", \"LONGITUDE\", \"LOCATION\", \"TIMEZONE\" from main.experiment_tweets_shortest where \"RETWEET\" = 0")

## @knitr closeDB

## @knitr other

#check for table existence
dbExistsTable(con, c("main","experiment_tweets_shortest"))
#expected TRUE

#writes df to the PostgreSQL database "postgres", table "cartable" 
#dbWriteTable(con, "cartable", 
#             value = df, append = TRUE, row.names = FALSE)

#query the data from postgreSQL 
single <- dbGetQuery(con, "SELECT * from main.experiment_tweets_shortest where \"USERNAME\" = 'Londs_'")
tweets <- dbGetQuery(con, "SELECT * from main.experiment_tweets_shortest")

#structure of tweet dataframe
str(single)

#close connection
#no need, done automatically
