# Attribute analysis - No of Devices







## Connect to the database first



```
## Loading required package: DBI
```

```
## [1] TRUE
```

##Get the tweets


##Display no of devices overall

```r
ggplot(data = users, aes(x = no_of_devices)) + geom_histogram(aes(fill = ..count..)) + 
    theme(legend.position = "none") + xlab("No of devices") + ylab("Count") + 
    scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](FactorAnalysis-NoOfDevices_files/figure-html/device-1.png)<!-- -->

##Display no of devices per continent

```r
ggplot(data = users, aes(x = no_of_devices)) + geom_histogram(aes(fill = ..count..)) + 
    theme(legend.position = "none") + facet_wrap(~continent) + xlab("No of devices") + 
    ylab("Count") + scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](FactorAnalysis-NoOfDevices_files/figure-html/device_continent-1.png)<!-- -->
##Display no of devices for only Africa

```r
a_users <- users[users$continent == "Africa", ]
ggplot(data = a_users, aes(x = no_of_devices)) + geom_histogram(aes(fill = ..count..)) + 
    theme(legend.position = "none") + facet_wrap(~continent) + xlab("No of devices") + 
    ylab("Count") + scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](FactorAnalysis-NoOfDevices_files/figure-html/device_continent_africa-1.png)<!-- -->

##Score the data

###first create a score

```r
factor_no <- 1
exp_no <- 1
period_no <- 1

sql <- paste("DELETE FROM main.experiment_user_score where factor_no = 1", sep = "")
dbSendQuery(con, sql)
```

```
## <PostgreSQLResult>
```

```r
sql <- paste("INSERT INTO main.experiment_user_score(experiment_no, period_no, userid, factor_no, idi_full)", 
    sep = "")
sql <- paste(sql, "select experiment_no, period_no, userid, 1,", sep = "")
sql <- paste(sql, "case when no_of_devices = 1 then 1 when no_of_devices = 2 then 0.5 else 0.5 - (no_of_devices * 0.05) end", 
    sep = "")
sql <- paste(sql, " from main.experiment_user", sep = "")
dbSendQuery(con, sql)
```

```
## <PostgreSQLResult>
```

###show results
No scaling required


```r
user.score <- dbGetQuery(con, "SELECT s.userid, s.idi_full, tz.continent from main.experiment_user_score s join main.experiment_user u on u.userid = s.userid left join main.timezone_r tz on tz.timezone = u.timezone where s.factor_no = 1 and s.experiment_no = u.experiment_no and s.period_no = u.period_no")

colnames(user.score) = c("userid", "idi", "continent")

ggplot(user.score, aes(x = continent, y = idi)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4)
```

![](FactorAnalysis-NoOfDevices_files/figure-html/score_n-1.png)<!-- -->

##Outlier detection
Use Tukey's method to update all scores that were outliers


```r
markoutlier <- function(x, exp_no, period_no, factor_no) {
    sql <- paste("update main.experiment_user_score set outlier_full=1", sep = "")
    sql <- paste(sql, " where userid='", x["userid"], "'", sep = "")
    sql <- paste(sql, " and experiment_no=", exp_no, sep = "")
    sql <- paste(sql, " and period_no=", period_no, sep = "")
    sql <- paste(sql, " and factor_no=", factor_no, sep = "")
    dbSendQuery(con, sql, echo = FALSE)
}

# TODO outliers per continent
continents <- unique(user.score$continent)

user.continent_score <- user.score
outlier <- boxplot.stats(user.continent_score$idi, coef = 1.5)$out
user.outlier <- user.continent_score[user.continent_score$idi %in% outlier, 
    ]
apply(user.outlier, 1, markoutlier, exp_no = exp_no, period_no = period_no, 
    factor_no = factor_no)
# na1 <- nrow(user.outlier) Outliers identified: na1 Propotion (%) of
# outliers: round(na1 / sum(!is.na(user.continent_score$idi))*100, 1)
```

Total outliers: 1129 out of 6846


