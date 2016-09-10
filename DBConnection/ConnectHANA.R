#install.packages("RODBC")

## @knitr connectDB
library(RODBC) 

#### connect to DB
myconn<-odbcConnect("FSOC", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=50000, DBMSencoding="UTF-8") 

## @knitr tweets_single_user
tweets.singleuser <- dbGetQuery(con, "SELECT * from twitter.experiment_tweets_shortest where \"USERNAME\" = 'Londs_'")

## @knitr tweets
tweets <- dbGetQuery(con, "SELECT \"ID\", \"CREATEDAT\", \"USERID\", \"RETWEET\", \"GEO_ENABLED\", \"LATITUDE\", \"LONGITUDE\", \"LOCATION\", \"TIMEZONE\" from twitter.experiment_tweets_shortest")

## @knitr tweet_content
tweets <- dbGetQuery(con, "SELECT \"USERNAME\", \"USERID\", \"CREATEDAT\", \"CONTENT\",\"GEO_ENABLED\", \"LATITUDE\", \"LONGITUDE\", \"LOCATION\", \"TIMEZONE\" from twitter.experiment_tweets_shortest where \"RETWEET\" = 0")

## @knitr closeDB
####close DB connection
close(myconn) 

## @knitr other

####get locale
Sys.getlocale()

#list all tables
tb <- sqlTables(myconn)

dataset <- sqlQuery(myconn, "select * from TWITTER.TEST_TWEETS;")
dataset

head(dataset)

