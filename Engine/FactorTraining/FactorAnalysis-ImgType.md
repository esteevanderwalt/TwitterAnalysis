# Attribute analysis - Type of Image







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


##Profile image type

```r
ggplot(data = users, aes(x = profile_image_type)) + geom_bar() + theme(legend.position = "none") + 
    xlab("Profile") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4")
```

![](FactorAnalysis-ImgType_files/figure-html/profile-1.png)<!-- -->

##Background image type

```r
ggplot(data = users, aes(x = background_image_type)) + geom_bar() + theme(legend.position = "none") + 
    xlab("Background") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4")
```

![](FactorAnalysis-ImgType_files/figure-html/background-1.png)<!-- -->


