# Attribute analysis - Friends vs Followers







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


##Friends vs Followers

```r
ggplot(users, aes(x = no_of_friends, y = no_of_followers)) + geom_point(aes(color = factor(continent)))
```

![](FactorAnalysis-FriendsFollowers_files/figure-html/ff-1.png)<!-- -->

##Friends vs Followers per continent

```r
ggplot(users, aes(x = no_of_friends, y = no_of_followers)) + geom_point(aes(color = factor(continent))) + 
    facet_wrap(~continent)
```

![](FactorAnalysis-FriendsFollowers_files/figure-html/ff_continent-1.png)<!-- -->



