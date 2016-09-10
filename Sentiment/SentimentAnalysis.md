# Sentiment Analysis with Twitter Data





The aim here is to perform sentiment analysis from the content within tweets. We have various options in processing the content: we can rely on NLP libraries or we can use the language processing capabilities built into the SAP HANA library. We will try both and visually show the results in a wordcloud to measure any noticable differences at first.
We will then create a wordcloud for a select few users and measure there distance from the main word cloud, ie how much does their talk differ to that from the corpus.

## Connect to the database first



```r
library(RPostgreSQL)
```

```
## Loading required package: DBI
```

```r
# create a connection save the password that we can 'hide' it as best as we
# can by collapsing it
pw <- {
    ""
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database note that 'con' will be used
# later in each connection to the database
con <- dbConnect(drv, dbname = "twitter", host = "localhost", port = 5432, user = "postgres", 
    password = "")

rm(pw)  # removes the password
```

Connection success: TRUE

##Pull data from the database
We will pull the full corpus but selective fields for analysis due to memory limitations.
Ignore any retweets as we deem this to not be the original content from the user itself and not their opinion.
(we still potentially have to exclude content from bots)


```r
tweets <- dbGetQuery(con, "SELECT \"USERNAME\", \"USERID\", \"CREATEDAT\", \"CONTENT\",\"GEO_ENABLED\", \"LATITUDE\", \"LONGITUDE\", \"LOCATION\", \"TIMEZONE\" from main.experiment_tweets_shortest where \"RETWEET\" = 0")
```

Total amount of tweets in the corpus: 3168560

Total unique users: 6782

Avg amount of tweets per user: 467.2014155

####Word clouds

Build a word cloud over full corpus


Build a word cloud for a few users


####Sentiment analysis

For the full corpus


For a few individual users


####Per continents

How many tweets have location enabled

How many tweets have location enabled per continent

Use timezone to determine continent/location iso geo points

User location to determine continent/location iso geo points

Word clouds for different continents over the full corpus

Sentiment per continent


####Per country

Word clouds for different continents over the full corpus

Sentiment per continent

##Close the Database connection

