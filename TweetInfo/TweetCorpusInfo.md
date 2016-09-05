# Tweet Corpus Information





## Connect to the database first

```r
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database note that 'con' will be used
# later in each connection to the database
con <- dbConnect(drv, dbname = "twitter", host = "localhost", port = 5432, user = "postgres", 
    password = "")
```

Connection success: TRUE

##Pull data from the database
We will only pull the data for one user to understand the structure and content initially


```r
tweets <- dbGetQuery(con, "SELECT * from main.experiment_tweets_shortest where \"USERNAME\" = 'Londs_'")
```

Amount of records retrieved: 1000
