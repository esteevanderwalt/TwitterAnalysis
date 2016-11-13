# Attribute analysis - TZ vs Avg Tweet Time







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


##Display avg tweet hour per timezone

```r
df <- data.frame(users$avg, users$timezone)
user.data <- df[complete.cases(df), ]
colnames(user.data) = c("avg", "timezone")
ggplot(user.data, aes(x = timezone, y = avg)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4)
```

![](FactorAnalysis-AvgTweetTime_files/figure-html/avg_tweet-1.png)<!-- -->

```r
# geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# geom_jitter(shape=16, position=position_jitter(0.2))
```

##Display avg tweet hour per continent

```r
df <- data.frame(users$avg, users$continent)
user.data <- df[complete.cases(df), ]
colnames(user.data) = c("avg", "continent")
ggplot(user.data, aes(x = continent, y = avg)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4)
```

![](FactorAnalysis-AvgTweetTime_files/figure-html/avg_tweet_continent-1.png)<!-- -->

```r
# geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# geom_jitter(shape=16, position=position_jitter(0.2))
```


