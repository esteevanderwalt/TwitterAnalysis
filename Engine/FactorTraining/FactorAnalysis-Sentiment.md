# Attribute analysis - Sentiment







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


##Display overall sentiment

```r
user.s <- users[users$sentiment != "NA", ]
ggplot(data = user.s, aes(x = sentiment)) + geom_bar() + theme(legend.position = "none") + 
    xlab("Sentiment") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4")
```

![](FactorAnalysis-Sentiment_files/figure-html/sentiment-1.png)<!-- -->

##Display sentiment per continent

```r
ggplot(data = user.s, aes(x = sentiment)) + geom_bar() + theme(legend.position = "none") + 
    facet_wrap(~continent) + xlab("Sentiment") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](FactorAnalysis-Sentiment_files/figure-html/sentiment_continent-1.png)<!-- -->
##Sentiment for only Africa

```r
a_users <- users[user.s$continent == "Africa", ]
ggplot(data = a_users, aes(x = sentiment)) + geom_bar() + theme(legend.position = "none") + 
    facet_wrap(~continent) + xlab("Sentiment") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](FactorAnalysis-Sentiment_files/figure-html/sentiment_continent_africa-1.png)<!-- -->
###Positive, Negative sentiment

##Display overall sentiment

```r
user.s <- users[users$sentiment_pos_neg != "NA", ]
ggplot(data = users, aes(x = sentiment_pos_neg)) + geom_bar() + theme(legend.position = "none") + 
    xlab("Sentiment") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4")
```

![](FactorAnalysis-Sentiment_files/figure-html/sentiment_pos_neg-1.png)<!-- -->

##Display sentiment per continent

```r
ggplot(data = user.s, aes(x = sentiment_pos_neg)) + geom_bar() + theme(legend.position = "none") + 
    facet_wrap(~continent) + xlab("Sentiment") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](FactorAnalysis-Sentiment_files/figure-html/sentiment_pos_neg_continent-1.png)<!-- -->
##Sentiment for only Africa

```r
a_user <- user.s[user.s$continent == "Africa", ]
ggplot(data = a_user, aes(x = sentiment_pos_neg)) + geom_bar() + theme(legend.position = "none") + 
    facet_wrap(~continent) + xlab("Sentiment") + ylab("Count") + scale_fill_gradient(low = "midnightblue", 
    high = "aquamarine4") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](FactorAnalysis-Sentiment_files/figure-html/sentiment_pos_neg_continent_africa-1.png)<!-- -->


