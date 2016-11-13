# Attribute analysis - Distances







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


###Display latitude, longitude vs timezone

```r
df <- data.frame(users$dist_latlon_vs_tz, users$continent)
user.data <- df[complete.cases(df), ]
colnames(user.data) = c("dist_latlon_vs_tz", "continent")
p2 <- ggplot(user.data, aes(x = continent, y = dist_latlon_vs_tz)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# geom_jitter(shape=16, position=position_jitter(0.2))
p2
```

![](FactorAnalysis-Distances_files/figure-html/ll_tz-1.png)<!-- -->
###Display latitude, longitude vs location

```r
df <- data.frame(users$dist_latlon_vs_loc, users$continent)
user.data <- df[complete.cases(df), ]
colnames(user.data) = c("dist_latlon_vs_loc", "continent")
p3 <- ggplot(user.data, aes(x = continent, y = dist_latlon_vs_loc)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# geom_jitter(shape=16, position=position_jitter(0.2))
p3
```

![](FactorAnalysis-Distances_files/figure-html/ll_loc-1.png)<!-- -->
###Display location vs timezone

```r
df <- data.frame(users$dist_tz_vs_loc, users$continent)
user.data <- df[complete.cases(df), ]
colnames(user.data) = c("dist_tz_vs_loc", "continent")
p4 <- ggplot(user.data, aes(x = continent, y = dist_tz_vs_loc)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# geom_jitter(shape=16, position=position_jitter(0.2))
p4
```

![](FactorAnalysis-Distances_files/figure-html/loc_tz-1.png)<!-- -->

##All in one grid

```r
grid.arrange(p2, p3, p4, nrow = 1)
```

![](FactorAnalysis-Distances_files/figure-html/grid-1.png)<!-- -->

##Combined results

```r
df <- data.frame((users$dist_latlon_vs_tz + users$dist_latlon_vs_loc + users$dist_tz_vs_loc)/3, 
    users$continent)
user.data <- df[complete.cases(df), ]
colnames(user.data) = c("dist", "continent")
ggplot(user.data, aes(x = continent, y = dist)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4)
```

![](FactorAnalysis-Distances_files/figure-html/loc_total-1.png)<!-- -->

```r
# geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# geom_jitter(shape=16, position=position_jitter(0.2))
```


