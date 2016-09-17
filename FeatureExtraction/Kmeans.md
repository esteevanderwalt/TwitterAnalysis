# K-means clustering







The K-means algorithm aims to choose centroids C that minimize the within cluster sum of squares objective function with a dataset X with n samples

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
# user = postgres for UBUNTU

rm(pw)  # removes the password

# Connection success:
dbExistsTable(con, c("main", "experiment_tweets_shortest"))
```

```
## [1] TRUE
```

##Get the tweets

```r
tweets <- dbGetQuery(con, "SELECT \"USERNAME\", \"USERID\", \"CREATEDAT\", \"GEO_ENABLED\", \"LATITUDE\", \"LONGITUDE\", \"LOCATION\", \"TIMEZONE\" from main.experiment_tweets_shortest where \"RETWEET\" = 0 and \"USERNAME\" in ('Londs_','AbeaChou','shift_comma3','HorizonCDT','ttenraBsucraMaD','maknaewon','oliviagarrett25','yallsop','Blackdolphin5','aaleyiahpoisson')")
```

Total amount of tweets in the corpus: 2789

Total unique users: 9
  
##Train the data

##The analysis here



