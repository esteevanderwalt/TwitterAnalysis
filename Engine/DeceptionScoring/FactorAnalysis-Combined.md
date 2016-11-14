# Attribute analysis - Combined







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

##Score the data

###first create a score

```r
factor_no <- 0
exp_no <- 1
period_no <- 1

sql <- paste("DELETE FROM main.experiment_user_score where factor_no = 0", sep = "")
dbSendQuery(con, sql)
```

```
## <PostgreSQLResult>
```

```r
sql <- paste("INSERT INTO main.experiment_user_score(experiment_no, period_no, userid, factor_no, idi_full)", 
    sep = "")
sql <- paste(sql, "select u1.experiment_no, u1.period_no, u1.userid, 0,", sep = "")
sql <- paste(sql, "(coalesce(u1.idi_full,0)+coalesce(u2.idi_full,0)+coalesce(u3.idi_full,0)+coalesce(u4.idi_full,0)+coalesce(u5.idi_full,0)+coalesce(u6.idi_full,0))/6 as idi_full", 
    sep = "")
sql <- paste(sql, " from main.experiment_user_score u1", sep = "")
sql <- paste(sql, " left join main.experiment_user_score u2 on u1.userid = u2.userid and u2.factor_no=2", 
    sep = "")
sql <- paste(sql, " left join main.experiment_user_score u3 on u1.userid = u3.userid and u3.factor_no=3", 
    sep = "")
sql <- paste(sql, " left join main.experiment_user_score u4 on u1.userid = u4.userid and u4.factor_no=4", 
    sep = "")
sql <- paste(sql, " left join main.experiment_user_score u5 on u1.userid = u5.userid and u5.factor_no=5", 
    sep = "")
sql <- paste(sql, " left join main.experiment_user_score u6 on u1.userid = u6.userid and u6.factor_no=6", 
    sep = "")
sql <- paste(sql, " where u1.factor_no=1", sep = "")
dbSendQuery(con, sql)
```

```
## <PostgreSQLResult>
```

###show results
No scaling required


```r
user.score <- dbGetQuery(con, "SELECT s.idi_full, tz.continent from main.experiment_user_score s join main.experiment_user u on u.userid = s.userid left join main.timezone_r tz on tz.timezone = u.timezone where s.factor_no = 0 and s.experiment_no = u.experiment_no and s.period_no = u.period_no")

colnames(user.score) = c("idi", "continent")

ggplot(user.score, aes(x = continent, y = idi)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4)
```

![](FactorAnalysis-Combined_files/figure-html/score_n-1.png)<!-- -->

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
```

```
## $`9`
## <PostgreSQLResult>
## 
## $`29`
## <PostgreSQLResult>
## 
## $`30`
## <PostgreSQLResult>
## 
## $`31`
## <PostgreSQLResult>
## 
## $`32`
## <PostgreSQLResult>
## 
## $`34`
## <PostgreSQLResult>
## 
## $`35`
## <PostgreSQLResult>
## 
## $`36`
## <PostgreSQLResult>
## 
## $`37`
## <PostgreSQLResult>
## 
## $`38`
## <PostgreSQLResult>
## 
## $`39`
## <PostgreSQLResult>
## 
## $`40`
## <PostgreSQLResult>
## 
## $`41`
## <PostgreSQLResult>
## 
## $`42`
## <PostgreSQLResult>
## 
## $`43`
## <PostgreSQLResult>
## 
## $`45`
## <PostgreSQLResult>
## 
## $`46`
## <PostgreSQLResult>
## 
## $`47`
## <PostgreSQLResult>
## 
## $`48`
## <PostgreSQLResult>
## 
## $`49`
## <PostgreSQLResult>
## 
## $`50`
## <PostgreSQLResult>
## 
## $`51`
## <PostgreSQLResult>
## 
## $`52`
## <PostgreSQLResult>
## 
## $`53`
## <PostgreSQLResult>
## 
## $`55`
## <PostgreSQLResult>
## 
## $`56`
## <PostgreSQLResult>
## 
## $`58`
## <PostgreSQLResult>
## 
## $`59`
## <PostgreSQLResult>
## 
## $`60`
## <PostgreSQLResult>
## 
## $`63`
## <PostgreSQLResult>
## 
## $`64`
## <PostgreSQLResult>
## 
## $`65`
## <PostgreSQLResult>
## 
## $`66`
## <PostgreSQLResult>
## 
## $`67`
## <PostgreSQLResult>
## 
## $`68`
## <PostgreSQLResult>
## 
## $`69`
## <PostgreSQLResult>
## 
## $`70`
## <PostgreSQLResult>
## 
## $`72`
## <PostgreSQLResult>
## 
## $`73`
## <PostgreSQLResult>
## 
## $`74`
## <PostgreSQLResult>
## 
## $`75`
## <PostgreSQLResult>
## 
## $`76`
## <PostgreSQLResult>
## 
## $`77`
## <PostgreSQLResult>
## 
## $`78`
## <PostgreSQLResult>
## 
## $`79`
## <PostgreSQLResult>
## 
## $`80`
## <PostgreSQLResult>
## 
## $`81`
## <PostgreSQLResult>
## 
## $`82`
## <PostgreSQLResult>
## 
## $`83`
## <PostgreSQLResult>
## 
## $`84`
## <PostgreSQLResult>
## 
## $`85`
## <PostgreSQLResult>
## 
## $`86`
## <PostgreSQLResult>
## 
## $`87`
## <PostgreSQLResult>
## 
## $`90`
## <PostgreSQLResult>
## 
## $`92`
## <PostgreSQLResult>
## 
## $`93`
## <PostgreSQLResult>
## 
## $`94`
## <PostgreSQLResult>
## 
## $`95`
## <PostgreSQLResult>
## 
## $`96`
## <PostgreSQLResult>
## 
## $`97`
## <PostgreSQLResult>
## 
## $`99`
## <PostgreSQLResult>
## 
## $`100`
## <PostgreSQLResult>
## 
## $`104`
## <PostgreSQLResult>
## 
## $`105`
## <PostgreSQLResult>
## 
## $`106`
## <PostgreSQLResult>
## 
## $`107`
## <PostgreSQLResult>
## 
## $`108`
## <PostgreSQLResult>
## 
## $`109`
## <PostgreSQLResult>
## 
## $`110`
## <PostgreSQLResult>
## 
## $`111`
## <PostgreSQLResult>
## 
## $`112`
## <PostgreSQLResult>
## 
## $`113`
## <PostgreSQLResult>
## 
## $`114`
## <PostgreSQLResult>
## 
## $`115`
## <PostgreSQLResult>
## 
## $`116`
## <PostgreSQLResult>
## 
## $`117`
## <PostgreSQLResult>
## 
## $`118`
## <PostgreSQLResult>
## 
## $`119`
## <PostgreSQLResult>
## 
## $`120`
## <PostgreSQLResult>
## 
## $`122`
## <PostgreSQLResult>
## 
## $`123`
## <PostgreSQLResult>
## 
## $`124`
## <PostgreSQLResult>
## 
## $`125`
## <PostgreSQLResult>
## 
## $`126`
## <PostgreSQLResult>
## 
## $`127`
## <PostgreSQLResult>
## 
## $`128`
## <PostgreSQLResult>
## 
## $`129`
## <PostgreSQLResult>
## 
## $`130`
## <PostgreSQLResult>
## 
## $`131`
## <PostgreSQLResult>
## 
## $`132`
## <PostgreSQLResult>
## 
## $`133`
## <PostgreSQLResult>
## 
## $`134`
## <PostgreSQLResult>
## 
## $`135`
## <PostgreSQLResult>
## 
## $`136`
## <PostgreSQLResult>
## 
## $`137`
## <PostgreSQLResult>
## 
## $`139`
## <PostgreSQLResult>
## 
## $`143`
## <PostgreSQLResult>
## 
## $`144`
## <PostgreSQLResult>
## 
## $`146`
## <PostgreSQLResult>
## 
## $`147`
## <PostgreSQLResult>
## 
## $`148`
## <PostgreSQLResult>
## 
## $`150`
## <PostgreSQLResult>
## 
## $`151`
## <PostgreSQLResult>
## 
## $`153`
## <PostgreSQLResult>
## 
## $`154`
## <PostgreSQLResult>
## 
## $`156`
## <PostgreSQLResult>
## 
## $`208`
## <PostgreSQLResult>
## 
## $`222`
## <PostgreSQLResult>
## 
## $`223`
## <PostgreSQLResult>
## 
## $`225`
## <PostgreSQLResult>
## 
## $`227`
## <PostgreSQLResult>
## 
## $`229`
## <PostgreSQLResult>
## 
## $`230`
## <PostgreSQLResult>
## 
## $`232`
## <PostgreSQLResult>
## 
## $`233`
## <PostgreSQLResult>
## 
## $`234`
## <PostgreSQLResult>
## 
## $`235`
## <PostgreSQLResult>
## 
## $`236`
## <PostgreSQLResult>
## 
## $`237`
## <PostgreSQLResult>
## 
## $`238`
## <PostgreSQLResult>
## 
## $`249`
## <PostgreSQLResult>
## 
## $`256`
## <PostgreSQLResult>
## 
## $`305`
## <PostgreSQLResult>
## 
## $`336`
## <PostgreSQLResult>
## 
## $`337`
## <PostgreSQLResult>
## 
## $`338`
## <PostgreSQLResult>
## 
## $`339`
## <PostgreSQLResult>
## 
## $`340`
## <PostgreSQLResult>
## 
## $`341`
## <PostgreSQLResult>
## 
## $`343`
## <PostgreSQLResult>
## 
## $`347`
## <PostgreSQLResult>
## 
## $`348`
## <PostgreSQLResult>
## 
## $`349`
## <PostgreSQLResult>
## 
## $`350`
## <PostgreSQLResult>
## 
## $`351`
## <PostgreSQLResult>
## 
## $`415`
## <PostgreSQLResult>
## 
## $`443`
## <PostgreSQLResult>
## 
## $`444`
## <PostgreSQLResult>
## 
## $`446`
## <PostgreSQLResult>
## 
## $`447`
## <PostgreSQLResult>
## 
## $`449`
## <PostgreSQLResult>
## 
## $`450`
## <PostgreSQLResult>
## 
## $`451`
## <PostgreSQLResult>
## 
## $`453`
## <PostgreSQLResult>
## 
## $`454`
## <PostgreSQLResult>
## 
## $`455`
## <PostgreSQLResult>
## 
## $`458`
## <PostgreSQLResult>
## 
## $`459`
## <PostgreSQLResult>
## 
## $`460`
## <PostgreSQLResult>
## 
## $`463`
## <PostgreSQLResult>
## 
## $`464`
## <PostgreSQLResult>
## 
## $`466`
## <PostgreSQLResult>
## 
## $`491`
## <PostgreSQLResult>
## 
## $`554`
## <PostgreSQLResult>
## 
## $`558`
## <PostgreSQLResult>
## 
## $`561`
## <PostgreSQLResult>
## 
## $`562`
## <PostgreSQLResult>
## 
## $`563`
## <PostgreSQLResult>
## 
## $`564`
## <PostgreSQLResult>
## 
## $`565`
## <PostgreSQLResult>
## 
## $`566`
## <PostgreSQLResult>
## 
## $`567`
## <PostgreSQLResult>
## 
## $`568`
## <PostgreSQLResult>
## 
## $`569`
## <PostgreSQLResult>
## 
## $`570`
## <PostgreSQLResult>
## 
## $`572`
## <PostgreSQLResult>
## 
## $`573`
## <PostgreSQLResult>
## 
## $`574`
## <PostgreSQLResult>
## 
## $`576`
## <PostgreSQLResult>
## 
## $`668`
## <PostgreSQLResult>
## 
## $`669`
## <PostgreSQLResult>
## 
## $`670`
## <PostgreSQLResult>
## 
## $`672`
## <PostgreSQLResult>
## 
## $`673`
## <PostgreSQLResult>
## 
## $`674`
## <PostgreSQLResult>
## 
## $`677`
## <PostgreSQLResult>
## 
## $`678`
## <PostgreSQLResult>
## 
## $`679`
## <PostgreSQLResult>
## 
## $`680`
## <PostgreSQLResult>
## 
## $`683`
## <PostgreSQLResult>
## 
## $`684`
## <PostgreSQLResult>
## 
## $`687`
## <PostgreSQLResult>
## 
## $`688`
## <PostgreSQLResult>
## 
## $`721`
## <PostgreSQLResult>
## 
## $`732`
## <PostgreSQLResult>
## 
## $`763`
## <PostgreSQLResult>
## 
## $`788`
## <PostgreSQLResult>
## 
## $`789`
## <PostgreSQLResult>
## 
## $`791`
## <PostgreSQLResult>
## 
## $`792`
## <PostgreSQLResult>
## 
## $`795`
## <PostgreSQLResult>
## 
## $`796`
## <PostgreSQLResult>
## 
## $`797`
## <PostgreSQLResult>
## 
## $`798`
## <PostgreSQLResult>
## 
## $`799`
## <PostgreSQLResult>
## 
## $`800`
## <PostgreSQLResult>
## 
## $`802`
## <PostgreSQLResult>
## 
## $`803`
## <PostgreSQLResult>
## 
## $`820`
## <PostgreSQLResult>
## 
## $`881`
## <PostgreSQLResult>
## 
## $`893`
## <PostgreSQLResult>
## 
## $`894`
## <PostgreSQLResult>
## 
## $`895`
## <PostgreSQLResult>
## 
## $`896`
## <PostgreSQLResult>
## 
## $`897`
## <PostgreSQLResult>
## 
## $`898`
## <PostgreSQLResult>
## 
## $`900`
## <PostgreSQLResult>
## 
## $`901`
## <PostgreSQLResult>
## 
## $`903`
## <PostgreSQLResult>
## 
## $`904`
## <PostgreSQLResult>
## 
## $`905`
## <PostgreSQLResult>
## 
## $`906`
## <PostgreSQLResult>
## 
## $`908`
## <PostgreSQLResult>
## 
## $`911`
## <PostgreSQLResult>
## 
## $`912`
## <PostgreSQLResult>
## 
## $`913`
## <PostgreSQLResult>
## 
## $`914`
## <PostgreSQLResult>
## 
## $`915`
## <PostgreSQLResult>
## 
## $`917`
## <PostgreSQLResult>
## 
## $`934`
## <PostgreSQLResult>
## 
## $`1009`
## <PostgreSQLResult>
## 
## $`1010`
## <PostgreSQLResult>
## 
## $`1011`
## <PostgreSQLResult>
## 
## $`1012`
## <PostgreSQLResult>
## 
## $`1013`
## <PostgreSQLResult>
## 
## $`1015`
## <PostgreSQLResult>
## 
## $`1016`
## <PostgreSQLResult>
## 
## $`1017`
## <PostgreSQLResult>
## 
## $`1018`
## <PostgreSQLResult>
## 
## $`1019`
## <PostgreSQLResult>
## 
## $`1021`
## <PostgreSQLResult>
## 
## $`1022`
## <PostgreSQLResult>
## 
## $`1023`
## <PostgreSQLResult>
## 
## $`1024`
## <PostgreSQLResult>
## 
## $`1025`
## <PostgreSQLResult>
## 
## $`1026`
## <PostgreSQLResult>
## 
## $`1027`
## <PostgreSQLResult>
## 
## $`1028`
## <PostgreSQLResult>
## 
## $`1049`
## <PostgreSQLResult>
## 
## $`1105`
## <PostgreSQLResult>
## 
## $`1122`
## <PostgreSQLResult>
## 
## $`1124`
## <PostgreSQLResult>
## 
## $`1125`
## <PostgreSQLResult>
## 
## $`1126`
## <PostgreSQLResult>
## 
## $`1128`
## <PostgreSQLResult>
## 
## $`1129`
## <PostgreSQLResult>
## 
## $`1131`
## <PostgreSQLResult>
## 
## $`1132`
## <PostgreSQLResult>
## 
## $`1133`
## <PostgreSQLResult>
## 
## $`1134`
## <PostgreSQLResult>
## 
## $`1136`
## <PostgreSQLResult>
## 
## $`1141`
## <PostgreSQLResult>
## 
## $`1153`
## <PostgreSQLResult>
## 
## $`1224`
## <PostgreSQLResult>
## 
## $`1225`
## <PostgreSQLResult>
## 
## $`1226`
## <PostgreSQLResult>
## 
## $`1227`
## <PostgreSQLResult>
## 
## $`1228`
## <PostgreSQLResult>
## 
## $`1230`
## <PostgreSQLResult>
## 
## $`1231`
## <PostgreSQLResult>
## 
## $`1232`
## <PostgreSQLResult>
## 
## $`1233`
## <PostgreSQLResult>
## 
## $`1235`
## <PostgreSQLResult>
## 
## $`1236`
## <PostgreSQLResult>
## 
## $`1239`
## <PostgreSQLResult>
## 
## $`1240`
## <PostgreSQLResult>
## 
## $`1241`
## <PostgreSQLResult>
## 
## $`1243`
## <PostgreSQLResult>
## 
## $`1244`
## <PostgreSQLResult>
## 
## $`1245`
## <PostgreSQLResult>
## 
## $`1247`
## <PostgreSQLResult>
## 
## $`1248`
## <PostgreSQLResult>
## 
## $`1249`
## <PostgreSQLResult>
## 
## $`1250`
## <PostgreSQLResult>
## 
## $`1251`
## <PostgreSQLResult>
## 
## $`1265`
## <PostgreSQLResult>
## 
## $`1291`
## <PostgreSQLResult>
## 
## $`1297`
## <PostgreSQLResult>
## 
## $`1336`
## <PostgreSQLResult>
## 
## $`1337`
## <PostgreSQLResult>
## 
## $`1338`
## <PostgreSQLResult>
## 
## $`1339`
## <PostgreSQLResult>
## 
## $`1341`
## <PostgreSQLResult>
## 
## $`1342`
## <PostgreSQLResult>
## 
## $`1343`
## <PostgreSQLResult>
## 
## $`1344`
## <PostgreSQLResult>
## 
## $`1345`
## <PostgreSQLResult>
## 
## $`1346`
## <PostgreSQLResult>
## 
## $`1347`
## <PostgreSQLResult>
## 
## $`1348`
## <PostgreSQLResult>
## 
## $`1350`
## <PostgreSQLResult>
## 
## $`1351`
## <PostgreSQLResult>
## 
## $`1352`
## <PostgreSQLResult>
## 
## $`1355`
## <PostgreSQLResult>
## 
## $`1357`
## <PostgreSQLResult>
## 
## $`1358`
## <PostgreSQLResult>
## 
## $`1359`
## <PostgreSQLResult>
## 
## $`1360`
## <PostgreSQLResult>
## 
## $`1362`
## <PostgreSQLResult>
## 
## $`1363`
## <PostgreSQLResult>
## 
## $`1403`
## <PostgreSQLResult>
## 
## $`1433`
## <PostgreSQLResult>
## 
## $`1444`
## <PostgreSQLResult>
## 
## $`1446`
## <PostgreSQLResult>
## 
## $`1453`
## <PostgreSQLResult>
## 
## $`1456`
## <PostgreSQLResult>
## 
## $`1457`
## <PostgreSQLResult>
## 
## $`1458`
## <PostgreSQLResult>
## 
## $`1461`
## <PostgreSQLResult>
## 
## $`1462`
## <PostgreSQLResult>
## 
## $`1463`
## <PostgreSQLResult>
## 
## $`1464`
## <PostgreSQLResult>
## 
## $`1465`
## <PostgreSQLResult>
## 
## $`1466`
## <PostgreSQLResult>
## 
## $`1467`
## <PostgreSQLResult>
## 
## $`1469`
## <PostgreSQLResult>
## 
## $`1470`
## <PostgreSQLResult>
## 
## $`1472`
## <PostgreSQLResult>
## 
## $`1474`
## <PostgreSQLResult>
## 
## $`1475`
## <PostgreSQLResult>
## 
## $`1569`
## <PostgreSQLResult>
## 
## $`1570`
## <PostgreSQLResult>
## 
## $`1571`
## <PostgreSQLResult>
## 
## $`1572`
## <PostgreSQLResult>
## 
## $`1574`
## <PostgreSQLResult>
## 
## $`1575`
## <PostgreSQLResult>
## 
## $`1576`
## <PostgreSQLResult>
## 
## $`1578`
## <PostgreSQLResult>
## 
## $`1580`
## <PostgreSQLResult>
## 
## $`1581`
## <PostgreSQLResult>
## 
## $`1582`
## <PostgreSQLResult>
## 
## $`1583`
## <PostgreSQLResult>
## 
## $`1584`
## <PostgreSQLResult>
## 
## $`1589`
## <PostgreSQLResult>
## 
## $`1637`
## <PostgreSQLResult>
## 
## $`1642`
## <PostgreSQLResult>
## 
## $`1649`
## <PostgreSQLResult>
## 
## $`1672`
## <PostgreSQLResult>
## 
## $`1678`
## <PostgreSQLResult>
## 
## $`1679`
## <PostgreSQLResult>
## 
## $`1680`
## <PostgreSQLResult>
## 
## $`1682`
## <PostgreSQLResult>
## 
## $`1683`
## <PostgreSQLResult>
## 
## $`1685`
## <PostgreSQLResult>
## 
## $`1686`
## <PostgreSQLResult>
## 
## $`1687`
## <PostgreSQLResult>
## 
## $`1688`
## <PostgreSQLResult>
## 
## $`1689`
## <PostgreSQLResult>
## 
## $`1690`
## <PostgreSQLResult>
## 
## $`1692`
## <PostgreSQLResult>
## 
## $`1693`
## <PostgreSQLResult>
## 
## $`1694`
## <PostgreSQLResult>
## 
## $`1695`
## <PostgreSQLResult>
## 
## $`1696`
## <PostgreSQLResult>
## 
## $`1697`
## <PostgreSQLResult>
## 
## $`1699`
## <PostgreSQLResult>
## 
## $`1701`
## <PostgreSQLResult>
## 
## $`1732`
## <PostgreSQLResult>
## 
## $`1783`
## <PostgreSQLResult>
## 
## $`1785`
## <PostgreSQLResult>
## 
## $`1791`
## <PostgreSQLResult>
## 
## $`1792`
## <PostgreSQLResult>
## 
## $`1793`
## <PostgreSQLResult>
## 
## $`1794`
## <PostgreSQLResult>
## 
## $`1795`
## <PostgreSQLResult>
## 
## $`1797`
## <PostgreSQLResult>
## 
## $`1799`
## <PostgreSQLResult>
## 
## $`1800`
## <PostgreSQLResult>
## 
## $`1801`
## <PostgreSQLResult>
## 
## $`1802`
## <PostgreSQLResult>
## 
## $`1804`
## <PostgreSQLResult>
## 
## $`1805`
## <PostgreSQLResult>
## 
## $`1806`
## <PostgreSQLResult>
## 
## $`1807`
## <PostgreSQLResult>
## 
## $`1809`
## <PostgreSQLResult>
## 
## $`1905`
## <PostgreSQLResult>
## 
## $`1906`
## <PostgreSQLResult>
## 
## $`1907`
## <PostgreSQLResult>
## 
## $`1908`
## <PostgreSQLResult>
## 
## $`1909`
## <PostgreSQLResult>
## 
## $`1910`
## <PostgreSQLResult>
## 
## $`1911`
## <PostgreSQLResult>
## 
## $`1912`
## <PostgreSQLResult>
## 
## $`1913`
## <PostgreSQLResult>
## 
## $`1914`
## <PostgreSQLResult>
## 
## $`1915`
## <PostgreSQLResult>
## 
## $`1916`
## <PostgreSQLResult>
## 
## $`1917`
## <PostgreSQLResult>
## 
## $`1918`
## <PostgreSQLResult>
## 
## $`1920`
## <PostgreSQLResult>
## 
## $`1921`
## <PostgreSQLResult>
## 
## $`1922`
## <PostgreSQLResult>
## 
## $`1999`
## <PostgreSQLResult>
## 
## $`2013`
## <PostgreSQLResult>
## 
## $`2015`
## <PostgreSQLResult>
## 
## $`2016`
## <PostgreSQLResult>
## 
## $`2017`
## <PostgreSQLResult>
## 
## $`2018`
## <PostgreSQLResult>
## 
## $`2020`
## <PostgreSQLResult>
## 
## $`2021`
## <PostgreSQLResult>
## 
## $`2022`
## <PostgreSQLResult>
## 
## $`2023`
## <PostgreSQLResult>
## 
## $`2024`
## <PostgreSQLResult>
## 
## $`2025`
## <PostgreSQLResult>
## 
## $`2026`
## <PostgreSQLResult>
## 
## $`2028`
## <PostgreSQLResult>
## 
## $`2029`
## <PostgreSQLResult>
## 
## $`2031`
## <PostgreSQLResult>
## 
## $`2033`
## <PostgreSQLResult>
## 
## $`2034`
## <PostgreSQLResult>
## 
## $`2035`
## <PostgreSQLResult>
## 
## $`2069`
## <PostgreSQLResult>
## 
## $`2105`
## <PostgreSQLResult>
## 
## $`2106`
## <PostgreSQLResult>
## 
## $`2123`
## <PostgreSQLResult>
## 
## $`2124`
## <PostgreSQLResult>
## 
## $`2125`
## <PostgreSQLResult>
## 
## $`2127`
## <PostgreSQLResult>
## 
## $`2128`
## <PostgreSQLResult>
## 
## $`2129`
## <PostgreSQLResult>
## 
## $`2130`
## <PostgreSQLResult>
## 
## $`2133`
## <PostgreSQLResult>
## 
## $`2134`
## <PostgreSQLResult>
## 
## $`2135`
## <PostgreSQLResult>
## 
## $`2136`
## <PostgreSQLResult>
## 
## $`2137`
## <PostgreSQLResult>
## 
## $`2138`
## <PostgreSQLResult>
## 
## $`2140`
## <PostgreSQLResult>
## 
## $`2141`
## <PostgreSQLResult>
## 
## $`2142`
## <PostgreSQLResult>
## 
## $`2143`
## <PostgreSQLResult>
## 
## $`2144`
## <PostgreSQLResult>
## 
## $`2145`
## <PostgreSQLResult>
## 
## $`2146`
## <PostgreSQLResult>
## 
## $`2154`
## <PostgreSQLResult>
## 
## $`2206`
## <PostgreSQLResult>
## 
## $`2244`
## <PostgreSQLResult>
## 
## $`2245`
## <PostgreSQLResult>
## 
## $`2246`
## <PostgreSQLResult>
## 
## $`2248`
## <PostgreSQLResult>
## 
## $`2250`
## <PostgreSQLResult>
## 
## $`2251`
## <PostgreSQLResult>
## 
## $`2252`
## <PostgreSQLResult>
## 
## $`2253`
## <PostgreSQLResult>
## 
## $`2254`
## <PostgreSQLResult>
## 
## $`2256`
## <PostgreSQLResult>
## 
## $`2257`
## <PostgreSQLResult>
## 
## $`2258`
## <PostgreSQLResult>
## 
## $`2259`
## <PostgreSQLResult>
## 
## $`2260`
## <PostgreSQLResult>
## 
## $`2286`
## <PostgreSQLResult>
## 
## $`2304`
## <PostgreSQLResult>
## 
## $`2338`
## <PostgreSQLResult>
## 
## $`2352`
## <PostgreSQLResult>
## 
## $`2358`
## <PostgreSQLResult>
## 
## $`2359`
## <PostgreSQLResult>
## 
## $`2362`
## <PostgreSQLResult>
## 
## $`2363`
## <PostgreSQLResult>
## 
## $`2364`
## <PostgreSQLResult>
## 
## $`2365`
## <PostgreSQLResult>
## 
## $`2367`
## <PostgreSQLResult>
## 
## $`2368`
## <PostgreSQLResult>
## 
## $`2369`
## <PostgreSQLResult>
## 
## $`2371`
## <PostgreSQLResult>
## 
## $`2373`
## <PostgreSQLResult>
## 
## $`2374`
## <PostgreSQLResult>
## 
## $`2392`
## <PostgreSQLResult>
## 
## $`2416`
## <PostgreSQLResult>
## 
## $`2450`
## <PostgreSQLResult>
## 
## $`2468`
## <PostgreSQLResult>
## 
## $`2470`
## <PostgreSQLResult>
## 
## $`2471`
## <PostgreSQLResult>
## 
## $`2472`
## <PostgreSQLResult>
## 
## $`2473`
## <PostgreSQLResult>
## 
## $`2475`
## <PostgreSQLResult>
## 
## $`2477`
## <PostgreSQLResult>
## 
## $`2478`
## <PostgreSQLResult>
## 
## $`2479`
## <PostgreSQLResult>
## 
## $`2480`
## <PostgreSQLResult>
## 
## $`2481`
## <PostgreSQLResult>
## 
## $`2482`
## <PostgreSQLResult>
## 
## $`2483`
## <PostgreSQLResult>
## 
## $`2484`
## <PostgreSQLResult>
## 
## $`2485`
## <PostgreSQLResult>
## 
## $`2486`
## <PostgreSQLResult>
## 
## $`2487`
## <PostgreSQLResult>
## 
## $`2579`
## <PostgreSQLResult>
## 
## $`2581`
## <PostgreSQLResult>
## 
## $`2583`
## <PostgreSQLResult>
## 
## $`2584`
## <PostgreSQLResult>
## 
## $`2586`
## <PostgreSQLResult>
## 
## $`2587`
## <PostgreSQLResult>
## 
## $`2589`
## <PostgreSQLResult>
## 
## $`2590`
## <PostgreSQLResult>
## 
## $`2592`
## <PostgreSQLResult>
## 
## $`2593`
## <PostgreSQLResult>
## 
## $`2594`
## <PostgreSQLResult>
## 
## $`2595`
## <PostgreSQLResult>
## 
## $`2596`
## <PostgreSQLResult>
## 
## $`2597`
## <PostgreSQLResult>
## 
## $`2598`
## <PostgreSQLResult>
## 
## $`2600`
## <PostgreSQLResult>
## 
## $`2611`
## <PostgreSQLResult>
## 
## $`2630`
## <PostgreSQLResult>
## 
## $`2681`
## <PostgreSQLResult>
## 
## $`2691`
## <PostgreSQLResult>
## 
## $`2692`
## <PostgreSQLResult>
## 
## $`2693`
## <PostgreSQLResult>
## 
## $`2695`
## <PostgreSQLResult>
## 
## $`2696`
## <PostgreSQLResult>
## 
## $`2697`
## <PostgreSQLResult>
## 
## $`2698`
## <PostgreSQLResult>
## 
## $`2700`
## <PostgreSQLResult>
## 
## $`2704`
## <PostgreSQLResult>
## 
## $`2705`
## <PostgreSQLResult>
## 
## $`2706`
## <PostgreSQLResult>
## 
## $`2708`
## <PostgreSQLResult>
## 
## $`2709`
## <PostgreSQLResult>
## 
## $`2710`
## <PostgreSQLResult>
## 
## $`2802`
## <PostgreSQLResult>
## 
## $`2803`
## <PostgreSQLResult>
## 
## $`2805`
## <PostgreSQLResult>
## 
## $`2806`
## <PostgreSQLResult>
## 
## $`2807`
## <PostgreSQLResult>
## 
## $`2808`
## <PostgreSQLResult>
## 
## $`2809`
## <PostgreSQLResult>
## 
## $`2811`
## <PostgreSQLResult>
## 
## $`2812`
## <PostgreSQLResult>
## 
## $`2813`
## <PostgreSQLResult>
## 
## $`2814`
## <PostgreSQLResult>
## 
## $`2815`
## <PostgreSQLResult>
## 
## $`2816`
## <PostgreSQLResult>
## 
## $`2817`
## <PostgreSQLResult>
## 
## $`2819`
## <PostgreSQLResult>
## 
## $`2820`
## <PostgreSQLResult>
## 
## $`2822`
## <PostgreSQLResult>
## 
## $`2823`
## <PostgreSQLResult>
## 
## $`2905`
## <PostgreSQLResult>
## 
## $`2914`
## <PostgreSQLResult>
## 
## $`2916`
## <PostgreSQLResult>
## 
## $`2917`
## <PostgreSQLResult>
## 
## $`2918`
## <PostgreSQLResult>
## 
## $`2919`
## <PostgreSQLResult>
## 
## $`2920`
## <PostgreSQLResult>
## 
## $`2921`
## <PostgreSQLResult>
## 
## $`2922`
## <PostgreSQLResult>
## 
## $`2924`
## <PostgreSQLResult>
## 
## $`2927`
## <PostgreSQLResult>
## 
## $`2929`
## <PostgreSQLResult>
## 
## $`2931`
## <PostgreSQLResult>
## 
## $`2932`
## <PostgreSQLResult>
## 
## $`2933`
## <PostgreSQLResult>
## 
## $`2934`
## <PostgreSQLResult>
## 
## $`2936`
## <PostgreSQLResult>
## 
## $`3032`
## <PostgreSQLResult>
## 
## $`3033`
## <PostgreSQLResult>
## 
## $`3034`
## <PostgreSQLResult>
## 
## $`3036`
## <PostgreSQLResult>
## 
## $`3040`
## <PostgreSQLResult>
## 
## $`3041`
## <PostgreSQLResult>
## 
## $`3044`
## <PostgreSQLResult>
## 
## $`3045`
## <PostgreSQLResult>
## 
## $`3046`
## <PostgreSQLResult>
## 
## $`3047`
## <PostgreSQLResult>
## 
## $`3048`
## <PostgreSQLResult>
## 
## $`3049`
## <PostgreSQLResult>
## 
## $`3060`
## <PostgreSQLResult>
## 
## $`3131`
## <PostgreSQLResult>
## 
## $`3135`
## <PostgreSQLResult>
## 
## $`3137`
## <PostgreSQLResult>
## 
## $`3138`
## <PostgreSQLResult>
## 
## $`3139`
## <PostgreSQLResult>
## 
## $`3140`
## <PostgreSQLResult>
## 
## $`3141`
## <PostgreSQLResult>
## 
## $`3142`
## <PostgreSQLResult>
## 
## $`3143`
## <PostgreSQLResult>
## 
## $`3144`
## <PostgreSQLResult>
## 
## $`3145`
## <PostgreSQLResult>
## 
## $`3146`
## <PostgreSQLResult>
## 
## $`3147`
## <PostgreSQLResult>
## 
## $`3148`
## <PostgreSQLResult>
## 
## $`3149`
## <PostgreSQLResult>
## 
## $`3150`
## <PostgreSQLResult>
## 
## $`3151`
## <PostgreSQLResult>
## 
## $`3153`
## <PostgreSQLResult>
## 
## $`3154`
## <PostgreSQLResult>
## 
## $`3155`
## <PostgreSQLResult>
## 
## $`3156`
## <PostgreSQLResult>
## 
## $`3157`
## <PostgreSQLResult>
## 
## $`3158`
## <PostgreSQLResult>
## 
## $`3159`
## <PostgreSQLResult>
## 
## $`3249`
## <PostgreSQLResult>
## 
## $`3250`
## <PostgreSQLResult>
## 
## $`3251`
## <PostgreSQLResult>
## 
## $`3252`
## <PostgreSQLResult>
## 
## $`3253`
## <PostgreSQLResult>
## 
## $`3254`
## <PostgreSQLResult>
## 
## $`3255`
## <PostgreSQLResult>
## 
## $`3256`
## <PostgreSQLResult>
## 
## $`3257`
## <PostgreSQLResult>
## 
## $`3258`
## <PostgreSQLResult>
## 
## $`3259`
## <PostgreSQLResult>
## 
## $`3260`
## <PostgreSQLResult>
## 
## $`3261`
## <PostgreSQLResult>
## 
## $`3263`
## <PostgreSQLResult>
## 
## $`3264`
## <PostgreSQLResult>
## 
## $`3265`
## <PostgreSQLResult>
## 
## $`3266`
## <PostgreSQLResult>
## 
## $`3267`
## <PostgreSQLResult>
## 
## $`3268`
## <PostgreSQLResult>
## 
## $`3269`
## <PostgreSQLResult>
## 
## $`3270`
## <PostgreSQLResult>
## 
## $`3292`
## <PostgreSQLResult>
## 
## $`3297`
## <PostgreSQLResult>
## 
## $`3326`
## <PostgreSQLResult>
## 
## $`3363`
## <PostgreSQLResult>
## 
## $`3365`
## <PostgreSQLResult>
## 
## $`3367`
## <PostgreSQLResult>
## 
## $`3368`
## <PostgreSQLResult>
## 
## $`3369`
## <PostgreSQLResult>
## 
## $`3370`
## <PostgreSQLResult>
## 
## $`3371`
## <PostgreSQLResult>
## 
## $`3372`
## <PostgreSQLResult>
## 
## $`3373`
## <PostgreSQLResult>
## 
## $`3374`
## <PostgreSQLResult>
## 
## $`3376`
## <PostgreSQLResult>
## 
## $`3378`
## <PostgreSQLResult>
## 
## $`3383`
## <PostgreSQLResult>
## 
## $`3468`
## <PostgreSQLResult>
## 
## $`3475`
## <PostgreSQLResult>
## 
## $`3481`
## <PostgreSQLResult>
## 
## $`3482`
## <PostgreSQLResult>
## 
## $`3483`
## <PostgreSQLResult>
## 
## $`3484`
## <PostgreSQLResult>
## 
## $`3485`
## <PostgreSQLResult>
## 
## $`3486`
## <PostgreSQLResult>
## 
## $`3488`
## <PostgreSQLResult>
## 
## $`3489`
## <PostgreSQLResult>
## 
## $`3490`
## <PostgreSQLResult>
## 
## $`3491`
## <PostgreSQLResult>
## 
## $`3492`
## <PostgreSQLResult>
## 
## $`3493`
## <PostgreSQLResult>
## 
## $`3496`
## <PostgreSQLResult>
## 
## $`3497`
## <PostgreSQLResult>
## 
## $`3587`
## <PostgreSQLResult>
## 
## $`3588`
## <PostgreSQLResult>
## 
## $`3589`
## <PostgreSQLResult>
## 
## $`3590`
## <PostgreSQLResult>
## 
## $`3591`
## <PostgreSQLResult>
## 
## $`3592`
## <PostgreSQLResult>
## 
## $`3593`
## <PostgreSQLResult>
## 
## $`3594`
## <PostgreSQLResult>
## 
## $`3595`
## <PostgreSQLResult>
## 
## $`3596`
## <PostgreSQLResult>
## 
## $`3597`
## <PostgreSQLResult>
## 
## $`3598`
## <PostgreSQLResult>
## 
## $`3599`
## <PostgreSQLResult>
## 
## $`3600`
## <PostgreSQLResult>
## 
## $`3602`
## <PostgreSQLResult>
## 
## $`3604`
## <PostgreSQLResult>
## 
## $`3605`
## <PostgreSQLResult>
## 
## $`3606`
## <PostgreSQLResult>
## 
## $`3607`
## <PostgreSQLResult>
## 
## $`3608`
## <PostgreSQLResult>
## 
## $`3635`
## <PostgreSQLResult>
## 
## $`3696`
## <PostgreSQLResult>
## 
## $`3697`
## <PostgreSQLResult>
## 
## $`3698`
## <PostgreSQLResult>
## 
## $`3699`
## <PostgreSQLResult>
## 
## $`3700`
## <PostgreSQLResult>
## 
## $`3701`
## <PostgreSQLResult>
## 
## $`3702`
## <PostgreSQLResult>
## 
## $`3703`
## <PostgreSQLResult>
## 
## $`3704`
## <PostgreSQLResult>
## 
## $`3705`
## <PostgreSQLResult>
## 
## $`3706`
## <PostgreSQLResult>
## 
## $`3707`
## <PostgreSQLResult>
## 
## $`3708`
## <PostgreSQLResult>
## 
## $`3710`
## <PostgreSQLResult>
## 
## $`3711`
## <PostgreSQLResult>
## 
## $`3712`
## <PostgreSQLResult>
## 
## $`3713`
## <PostgreSQLResult>
## 
## $`3714`
## <PostgreSQLResult>
## 
## $`3715`
## <PostgreSQLResult>
## 
## $`3716`
## <PostgreSQLResult>
## 
## $`3717`
## <PostgreSQLResult>
## 
## $`3718`
## <PostgreSQLResult>
## 
## $`3719`
## <PostgreSQLResult>
## 
## $`3720`
## <PostgreSQLResult>
## 
## $`3808`
## <PostgreSQLResult>
## 
## $`3819`
## <PostgreSQLResult>
## 
## $`3820`
## <PostgreSQLResult>
## 
## $`3821`
## <PostgreSQLResult>
## 
## $`3822`
## <PostgreSQLResult>
## 
## $`3823`
## <PostgreSQLResult>
## 
## $`3824`
## <PostgreSQLResult>
## 
## $`3825`
## <PostgreSQLResult>
## 
## $`3827`
## <PostgreSQLResult>
## 
## $`3829`
## <PostgreSQLResult>
## 
## $`3831`
## <PostgreSQLResult>
## 
## $`3832`
## <PostgreSQLResult>
## 
## $`3834`
## <PostgreSQLResult>
## 
## $`3870`
## <PostgreSQLResult>
## 
## $`3872`
## <PostgreSQLResult>
## 
## $`3923`
## <PostgreSQLResult>
## 
## $`3924`
## <PostgreSQLResult>
## 
## $`3925`
## <PostgreSQLResult>
## 
## $`3926`
## <PostgreSQLResult>
## 
## $`3927`
## <PostgreSQLResult>
## 
## $`3930`
## <PostgreSQLResult>
## 
## $`3931`
## <PostgreSQLResult>
## 
## $`3932`
## <PostgreSQLResult>
## 
## $`3933`
## <PostgreSQLResult>
## 
## $`3934`
## <PostgreSQLResult>
## 
## $`3935`
## <PostgreSQLResult>
## 
## $`3936`
## <PostgreSQLResult>
## 
## $`3937`
## <PostgreSQLResult>
## 
## $`3939`
## <PostgreSQLResult>
## 
## $`3940`
## <PostgreSQLResult>
## 
## $`3941`
## <PostgreSQLResult>
## 
## $`3943`
## <PostgreSQLResult>
## 
## $`3944`
## <PostgreSQLResult>
## 
## $`3945`
## <PostgreSQLResult>
## 
## $`3946`
## <PostgreSQLResult>
## 
## $`4014`
## <PostgreSQLResult>
## 
## $`4038`
## <PostgreSQLResult>
## 
## $`4039`
## <PostgreSQLResult>
## 
## $`4040`
## <PostgreSQLResult>
## 
## $`4042`
## <PostgreSQLResult>
## 
## $`4044`
## <PostgreSQLResult>
## 
## $`4045`
## <PostgreSQLResult>
## 
## $`4046`
## <PostgreSQLResult>
## 
## $`4047`
## <PostgreSQLResult>
## 
## $`4048`
## <PostgreSQLResult>
## 
## $`4049`
## <PostgreSQLResult>
## 
## $`4050`
## <PostgreSQLResult>
## 
## $`4051`
## <PostgreSQLResult>
## 
## $`4052`
## <PostgreSQLResult>
## 
## $`4053`
## <PostgreSQLResult>
## 
## $`4054`
## <PostgreSQLResult>
## 
## $`4055`
## <PostgreSQLResult>
## 
## $`4056`
## <PostgreSQLResult>
## 
## $`4057`
## <PostgreSQLResult>
## 
## $`4058`
## <PostgreSQLResult>
## 
## $`4062`
## <PostgreSQLResult>
## 
## $`4078`
## <PostgreSQLResult>
## 
## $`4158`
## <PostgreSQLResult>
## 
## $`4160`
## <PostgreSQLResult>
## 
## $`4161`
## <PostgreSQLResult>
## 
## $`4163`
## <PostgreSQLResult>
## 
## $`4165`
## <PostgreSQLResult>
## 
## $`4166`
## <PostgreSQLResult>
## 
## $`4167`
## <PostgreSQLResult>
## 
## $`4168`
## <PostgreSQLResult>
## 
## $`4169`
## <PostgreSQLResult>
## 
## $`4172`
## <PostgreSQLResult>
## 
## $`4228`
## <PostgreSQLResult>
## 
## $`4271`
## <PostgreSQLResult>
## 
## $`4274`
## <PostgreSQLResult>
## 
## $`4275`
## <PostgreSQLResult>
## 
## $`4276`
## <PostgreSQLResult>
## 
## $`4277`
## <PostgreSQLResult>
## 
## $`4278`
## <PostgreSQLResult>
## 
## $`4279`
## <PostgreSQLResult>
## 
## $`4280`
## <PostgreSQLResult>
## 
## $`4281`
## <PostgreSQLResult>
## 
## $`4283`
## <PostgreSQLResult>
## 
## $`4284`
## <PostgreSQLResult>
## 
## $`4285`
## <PostgreSQLResult>
## 
## $`4298`
## <PostgreSQLResult>
## 
## $`4309`
## <PostgreSQLResult>
## 
## $`4335`
## <PostgreSQLResult>
## 
## $`4369`
## <PostgreSQLResult>
## 
## $`4370`
## <PostgreSQLResult>
## 
## $`4371`
## <PostgreSQLResult>
## 
## $`4372`
## <PostgreSQLResult>
## 
## $`4374`
## <PostgreSQLResult>
## 
## $`4375`
## <PostgreSQLResult>
## 
## $`4377`
## <PostgreSQLResult>
## 
## $`4378`
## <PostgreSQLResult>
## 
## $`4379`
## <PostgreSQLResult>
## 
## $`4380`
## <PostgreSQLResult>
## 
## $`4383`
## <PostgreSQLResult>
## 
## $`4385`
## <PostgreSQLResult>
## 
## $`4386`
## <PostgreSQLResult>
## 
## $`4387`
## <PostgreSQLResult>
## 
## $`4388`
## <PostgreSQLResult>
## 
## $`4389`
## <PostgreSQLResult>
## 
## $`4390`
## <PostgreSQLResult>
## 
## $`4391`
## <PostgreSQLResult>
## 
## $`4392`
## <PostgreSQLResult>
## 
## $`4394`
## <PostgreSQLResult>
## 
## $`4395`
## <PostgreSQLResult>
## 
## $`4396`
## <PostgreSQLResult>
## 
## $`4429`
## <PostgreSQLResult>
## 
## $`4439`
## <PostgreSQLResult>
## 
## $`4481`
## <PostgreSQLResult>
## 
## $`4483`
## <PostgreSQLResult>
## 
## $`4486`
## <PostgreSQLResult>
## 
## $`4488`
## <PostgreSQLResult>
## 
## $`4489`
## <PostgreSQLResult>
## 
## $`4490`
## <PostgreSQLResult>
## 
## $`4492`
## <PostgreSQLResult>
## 
## $`4493`
## <PostgreSQLResult>
## 
## $`4494`
## <PostgreSQLResult>
## 
## $`4496`
## <PostgreSQLResult>
## 
## $`4497`
## <PostgreSQLResult>
## 
## $`4498`
## <PostgreSQLResult>
## 
## $`4499`
## <PostgreSQLResult>
## 
## $`4500`
## <PostgreSQLResult>
## 
## $`4501`
## <PostgreSQLResult>
## 
## $`4502`
## <PostgreSQLResult>
## 
## $`4503`
## <PostgreSQLResult>
## 
## $`4504`
## <PostgreSQLResult>
## 
## $`4505`
## <PostgreSQLResult>
## 
## $`4506`
## <PostgreSQLResult>
## 
## $`4507`
## <PostgreSQLResult>
## 
## $`4548`
## <PostgreSQLResult>
## 
## $`4579`
## <PostgreSQLResult>
## 
## $`4607`
## <PostgreSQLResult>
## 
## $`4609`
## <PostgreSQLResult>
## 
## $`4610`
## <PostgreSQLResult>
## 
## $`4611`
## <PostgreSQLResult>
## 
## $`4612`
## <PostgreSQLResult>
## 
## $`4614`
## <PostgreSQLResult>
## 
## $`4617`
## <PostgreSQLResult>
## 
## $`4618`
## <PostgreSQLResult>
## 
## $`4619`
## <PostgreSQLResult>
## 
## $`4620`
## <PostgreSQLResult>
## 
## $`4622`
## <PostgreSQLResult>
## 
## $`4625`
## <PostgreSQLResult>
## 
## $`4660`
## <PostgreSQLResult>
## 
## $`4682`
## <PostgreSQLResult>
## 
## $`4713`
## <PostgreSQLResult>
## 
## $`4714`
## <PostgreSQLResult>
## 
## $`4715`
## <PostgreSQLResult>
## 
## $`4716`
## <PostgreSQLResult>
## 
## $`4717`
## <PostgreSQLResult>
## 
## $`4718`
## <PostgreSQLResult>
## 
## $`4720`
## <PostgreSQLResult>
## 
## $`4721`
## <PostgreSQLResult>
## 
## $`4722`
## <PostgreSQLResult>
## 
## $`4723`
## <PostgreSQLResult>
## 
## $`4724`
## <PostgreSQLResult>
## 
## $`4727`
## <PostgreSQLResult>
## 
## $`4728`
## <PostgreSQLResult>
## 
## $`4729`
## <PostgreSQLResult>
## 
## $`4730`
## <PostgreSQLResult>
## 
## $`4731`
## <PostgreSQLResult>
## 
## $`4732`
## <PostgreSQLResult>
## 
## $`4733`
## <PostgreSQLResult>
## 
## $`4734`
## <PostgreSQLResult>
## 
## $`4741`
## <PostgreSQLResult>
## 
## $`4767`
## <PostgreSQLResult>
## 
## $`4809`
## <PostgreSQLResult>
## 
## $`4820`
## <PostgreSQLResult>
## 
## $`4823`
## <PostgreSQLResult>
## 
## $`4824`
## <PostgreSQLResult>
## 
## $`4825`
## <PostgreSQLResult>
## 
## $`4826`
## <PostgreSQLResult>
## 
## $`4827`
## <PostgreSQLResult>
## 
## $`4828`
## <PostgreSQLResult>
## 
## $`4831`
## <PostgreSQLResult>
## 
## $`4832`
## <PostgreSQLResult>
## 
## $`4833`
## <PostgreSQLResult>
## 
## $`4834`
## <PostgreSQLResult>
## 
## $`4835`
## <PostgreSQLResult>
## 
## $`4838`
## <PostgreSQLResult>
## 
## $`4840`
## <PostgreSQLResult>
## 
## $`4841`
## <PostgreSQLResult>
## 
## $`4842`
## <PostgreSQLResult>
## 
## $`4843`
## <PostgreSQLResult>
## 
## $`4844`
## <PostgreSQLResult>
## 
## $`4845`
## <PostgreSQLResult>
## 
## $`4846`
## <PostgreSQLResult>
## 
## $`4876`
## <PostgreSQLResult>
## 
## $`4932`
## <PostgreSQLResult>
## 
## $`4933`
## <PostgreSQLResult>
## 
## $`4934`
## <PostgreSQLResult>
## 
## $`4935`
## <PostgreSQLResult>
## 
## $`4936`
## <PostgreSQLResult>
## 
## $`4937`
## <PostgreSQLResult>
## 
## $`4938`
## <PostgreSQLResult>
## 
## $`4941`
## <PostgreSQLResult>
## 
## $`4942`
## <PostgreSQLResult>
## 
## $`4944`
## <PostgreSQLResult>
## 
## $`4945`
## <PostgreSQLResult>
## 
## $`4946`
## <PostgreSQLResult>
## 
## $`4947`
## <PostgreSQLResult>
## 
## $`4948`
## <PostgreSQLResult>
## 
## $`4949`
## <PostgreSQLResult>
## 
## $`4950`
## <PostgreSQLResult>
## 
## $`4952`
## <PostgreSQLResult>
## 
## $`4953`
## <PostgreSQLResult>
## 
## $`4954`
## <PostgreSQLResult>
## 
## $`4955`
## <PostgreSQLResult>
## 
## $`4956`
## <PostgreSQLResult>
## 
## $`4957`
## <PostgreSQLResult>
## 
## $`5055`
## <PostgreSQLResult>
## 
## $`5057`
## <PostgreSQLResult>
## 
## $`5058`
## <PostgreSQLResult>
## 
## $`5059`
## <PostgreSQLResult>
## 
## $`5060`
## <PostgreSQLResult>
## 
## $`5061`
## <PostgreSQLResult>
## 
## $`5062`
## <PostgreSQLResult>
## 
## $`5063`
## <PostgreSQLResult>
## 
## $`5064`
## <PostgreSQLResult>
## 
## $`5065`
## <PostgreSQLResult>
## 
## $`5066`
## <PostgreSQLResult>
## 
## $`5067`
## <PostgreSQLResult>
## 
## $`5068`
## <PostgreSQLResult>
## 
## $`5069`
## <PostgreSQLResult>
## 
## $`5070`
## <PostgreSQLResult>
## 
## $`5147`
## <PostgreSQLResult>
## 
## $`5154`
## <PostgreSQLResult>
## 
## $`5163`
## <PostgreSQLResult>
## 
## $`5166`
## <PostgreSQLResult>
## 
## $`5168`
## <PostgreSQLResult>
## 
## $`5170`
## <PostgreSQLResult>
## 
## $`5173`
## <PostgreSQLResult>
## 
## $`5175`
## <PostgreSQLResult>
## 
## $`5176`
## <PostgreSQLResult>
## 
## $`5177`
## <PostgreSQLResult>
## 
## $`5178`
## <PostgreSQLResult>
## 
## $`5181`
## <PostgreSQLResult>
## 
## $`5182`
## <PostgreSQLResult>
## 
## $`5183`
## <PostgreSQLResult>
## 
## $`5268`
## <PostgreSQLResult>
## 
## $`5270`
## <PostgreSQLResult>
## 
## $`5271`
## <PostgreSQLResult>
## 
## $`5272`
## <PostgreSQLResult>
## 
## $`5273`
## <PostgreSQLResult>
## 
## $`5274`
## <PostgreSQLResult>
## 
## $`5276`
## <PostgreSQLResult>
## 
## $`5278`
## <PostgreSQLResult>
## 
## $`5280`
## <PostgreSQLResult>
## 
## $`5281`
## <PostgreSQLResult>
## 
## $`5282`
## <PostgreSQLResult>
## 
## $`5285`
## <PostgreSQLResult>
## 
## $`5287`
## <PostgreSQLResult>
## 
## $`5290`
## <PostgreSQLResult>
## 
## $`5291`
## <PostgreSQLResult>
## 
## $`5292`
## <PostgreSQLResult>
## 
## $`5293`
## <PostgreSQLResult>
## 
## $`5385`
## <PostgreSQLResult>
## 
## $`5386`
## <PostgreSQLResult>
## 
## $`5387`
## <PostgreSQLResult>
## 
## $`5389`
## <PostgreSQLResult>
## 
## $`5392`
## <PostgreSQLResult>
## 
## $`5393`
## <PostgreSQLResult>
## 
## $`5394`
## <PostgreSQLResult>
## 
## $`5395`
## <PostgreSQLResult>
## 
## $`5397`
## <PostgreSQLResult>
## 
## $`5398`
## <PostgreSQLResult>
## 
## $`5399`
## <PostgreSQLResult>
## 
## $`5401`
## <PostgreSQLResult>
## 
## $`5403`
## <PostgreSQLResult>
## 
## $`5406`
## <PostgreSQLResult>
## 
## $`5490`
## <PostgreSQLResult>
## 
## $`5491`
## <PostgreSQLResult>
## 
## $`5492`
## <PostgreSQLResult>
## 
## $`5493`
## <PostgreSQLResult>
## 
## $`5494`
## <PostgreSQLResult>
## 
## $`5495`
## <PostgreSQLResult>
## 
## $`5496`
## <PostgreSQLResult>
## 
## $`5499`
## <PostgreSQLResult>
## 
## $`5500`
## <PostgreSQLResult>
## 
## $`5502`
## <PostgreSQLResult>
## 
## $`5503`
## <PostgreSQLResult>
## 
## $`5504`
## <PostgreSQLResult>
## 
## $`5505`
## <PostgreSQLResult>
## 
## $`5506`
## <PostgreSQLResult>
## 
## $`5507`
## <PostgreSQLResult>
## 
## $`5510`
## <PostgreSQLResult>
## 
## $`5511`
## <PostgreSQLResult>
## 
## $`5512`
## <PostgreSQLResult>
## 
## $`5513`
## <PostgreSQLResult>
## 
## $`5514`
## <PostgreSQLResult>
## 
## $`5515`
## <PostgreSQLResult>
## 
## $`5516`
## <PostgreSQLResult>
## 
## $`5551`
## <PostgreSQLResult>
## 
## $`5570`
## <PostgreSQLResult>
## 
## $`5614`
## <PostgreSQLResult>
## 
## $`5615`
## <PostgreSQLResult>
## 
## $`5616`
## <PostgreSQLResult>
## 
## $`5617`
## <PostgreSQLResult>
## 
## $`5620`
## <PostgreSQLResult>
## 
## $`5621`
## <PostgreSQLResult>
## 
## $`5623`
## <PostgreSQLResult>
## 
## $`5625`
## <PostgreSQLResult>
## 
## $`5626`
## <PostgreSQLResult>
## 
## $`5627`
## <PostgreSQLResult>
## 
## $`5628`
## <PostgreSQLResult>
## 
## $`5629`
## <PostgreSQLResult>
## 
## $`5630`
## <PostgreSQLResult>
## 
## $`5659`
## <PostgreSQLResult>
## 
## $`5720`
## <PostgreSQLResult>
## 
## $`5721`
## <PostgreSQLResult>
## 
## $`5722`
## <PostgreSQLResult>
## 
## $`5726`
## <PostgreSQLResult>
## 
## $`5727`
## <PostgreSQLResult>
## 
## $`5731`
## <PostgreSQLResult>
## 
## $`5734`
## <PostgreSQLResult>
## 
## $`5735`
## <PostgreSQLResult>
## 
## $`5736`
## <PostgreSQLResult>
## 
## $`5737`
## <PostgreSQLResult>
## 
## $`5738`
## <PostgreSQLResult>
## 
## $`5740`
## <PostgreSQLResult>
## 
## $`5741`
## <PostgreSQLResult>
## 
## $`5742`
## <PostgreSQLResult>
## 
## $`5743`
## <PostgreSQLResult>
## 
## $`5754`
## <PostgreSQLResult>
## 
## $`5760`
## <PostgreSQLResult>
## 
## $`5836`
## <PostgreSQLResult>
## 
## $`5837`
## <PostgreSQLResult>
## 
## $`5838`
## <PostgreSQLResult>
## 
## $`5840`
## <PostgreSQLResult>
## 
## $`5841`
## <PostgreSQLResult>
## 
## $`5843`
## <PostgreSQLResult>
## 
## $`5844`
## <PostgreSQLResult>
## 
## $`5845`
## <PostgreSQLResult>
## 
## $`5846`
## <PostgreSQLResult>
## 
## $`5847`
## <PostgreSQLResult>
## 
## $`5848`
## <PostgreSQLResult>
## 
## $`5853`
## <PostgreSQLResult>
## 
## $`5854`
## <PostgreSQLResult>
## 
## $`5855`
## <PostgreSQLResult>
## 
## $`5856`
## <PostgreSQLResult>
## 
## $`5949`
## <PostgreSQLResult>
## 
## $`5950`
## <PostgreSQLResult>
## 
## $`5951`
## <PostgreSQLResult>
## 
## $`5952`
## <PostgreSQLResult>
## 
## $`5953`
## <PostgreSQLResult>
## 
## $`5954`
## <PostgreSQLResult>
## 
## $`5957`
## <PostgreSQLResult>
## 
## $`5958`
## <PostgreSQLResult>
## 
## $`5959`
## <PostgreSQLResult>
## 
## $`5960`
## <PostgreSQLResult>
## 
## $`5961`
## <PostgreSQLResult>
## 
## $`5962`
## <PostgreSQLResult>
## 
## $`5966`
## <PostgreSQLResult>
## 
## $`5967`
## <PostgreSQLResult>
## 
## $`5968`
## <PostgreSQLResult>
## 
## $`5969`
## <PostgreSQLResult>
## 
## $`6003`
## <PostgreSQLResult>
## 
## $`6050`
## <PostgreSQLResult>
## 
## $`6051`
## <PostgreSQLResult>
## 
## $`6053`
## <PostgreSQLResult>
## 
## $`6054`
## <PostgreSQLResult>
## 
## $`6057`
## <PostgreSQLResult>
## 
## $`6058`
## <PostgreSQLResult>
## 
## $`6059`
## <PostgreSQLResult>
## 
## $`6060`
## <PostgreSQLResult>
## 
## $`6064`
## <PostgreSQLResult>
## 
## $`6065`
## <PostgreSQLResult>
## 
## $`6066`
## <PostgreSQLResult>
## 
## $`6067`
## <PostgreSQLResult>
## 
## $`6068`
## <PostgreSQLResult>
## 
## $`6069`
## <PostgreSQLResult>
## 
## $`6070`
## <PostgreSQLResult>
## 
## $`6071`
## <PostgreSQLResult>
## 
## $`6072`
## <PostgreSQLResult>
## 
## $`6073`
## <PostgreSQLResult>
## 
## $`6074`
## <PostgreSQLResult>
## 
## $`6075`
## <PostgreSQLResult>
## 
## $`6080`
## <PostgreSQLResult>
## 
## $`6093`
## <PostgreSQLResult>
## 
## $`6126`
## <PostgreSQLResult>
## 
## $`6178`
## <PostgreSQLResult>
## 
## $`6180`
## <PostgreSQLResult>
## 
## $`6181`
## <PostgreSQLResult>
## 
## $`6182`
## <PostgreSQLResult>
## 
## $`6184`
## <PostgreSQLResult>
## 
## $`6185`
## <PostgreSQLResult>
## 
## $`6186`
## <PostgreSQLResult>
## 
## $`6189`
## <PostgreSQLResult>
## 
## $`6191`
## <PostgreSQLResult>
## 
## $`6192`
## <PostgreSQLResult>
## 
## $`6193`
## <PostgreSQLResult>
## 
## $`6282`
## <PostgreSQLResult>
## 
## $`6284`
## <PostgreSQLResult>
## 
## $`6288`
## <PostgreSQLResult>
## 
## $`6289`
## <PostgreSQLResult>
## 
## $`6290`
## <PostgreSQLResult>
## 
## $`6293`
## <PostgreSQLResult>
## 
## $`6294`
## <PostgreSQLResult>
## 
## $`6295`
## <PostgreSQLResult>
## 
## $`6296`
## <PostgreSQLResult>
## 
## $`6297`
## <PostgreSQLResult>
## 
## $`6298`
## <PostgreSQLResult>
## 
## $`6299`
## <PostgreSQLResult>
## 
## $`6301`
## <PostgreSQLResult>
## 
## $`6302`
## <PostgreSQLResult>
## 
## $`6304`
## <PostgreSQLResult>
## 
## $`6306`
## <PostgreSQLResult>
## 
## $`6394`
## <PostgreSQLResult>
## 
## $`6398`
## <PostgreSQLResult>
## 
## $`6399`
## <PostgreSQLResult>
## 
## $`6400`
## <PostgreSQLResult>
## 
## $`6401`
## <PostgreSQLResult>
## 
## $`6402`
## <PostgreSQLResult>
## 
## $`6403`
## <PostgreSQLResult>
## 
## $`6404`
## <PostgreSQLResult>
## 
## $`6405`
## <PostgreSQLResult>
## 
## $`6406`
## <PostgreSQLResult>
## 
## $`6407`
## <PostgreSQLResult>
## 
## $`6409`
## <PostgreSQLResult>
## 
## $`6411`
## <PostgreSQLResult>
## 
## $`6412`
## <PostgreSQLResult>
## 
## $`6413`
## <PostgreSQLResult>
## 
## $`6418`
## <PostgreSQLResult>
## 
## $`6445`
## <PostgreSQLResult>
## 
## $`6477`
## <PostgreSQLResult>
## 
## $`6478`
## <PostgreSQLResult>
## 
## $`6496`
## <PostgreSQLResult>
## 
## $`6501`
## <PostgreSQLResult>
## 
## $`6505`
## <PostgreSQLResult>
## 
## $`6506`
## <PostgreSQLResult>
## 
## $`6507`
## <PostgreSQLResult>
## 
## $`6509`
## <PostgreSQLResult>
## 
## $`6510`
## <PostgreSQLResult>
## 
## $`6511`
## <PostgreSQLResult>
## 
## $`6512`
## <PostgreSQLResult>
## 
## $`6514`
## <PostgreSQLResult>
## 
## $`6515`
## <PostgreSQLResult>
## 
## $`6516`
## <PostgreSQLResult>
## 
## $`6517`
## <PostgreSQLResult>
## 
## $`6519`
## <PostgreSQLResult>
## 
## $`6521`
## <PostgreSQLResult>
## 
## $`6522`
## <PostgreSQLResult>
## 
## $`6523`
## <PostgreSQLResult>
## 
## $`6524`
## <PostgreSQLResult>
## 
## $`6525`
## <PostgreSQLResult>
## 
## $`6526`
## <PostgreSQLResult>
## 
## $`6527`
## <PostgreSQLResult>
## 
## $`6531`
## <PostgreSQLResult>
## 
## $`6569`
## <PostgreSQLResult>
## 
## $`6570`
## <PostgreSQLResult>
## 
## $`6572`
## <PostgreSQLResult>
## 
## $`6647`
## <PostgreSQLResult>
## 
## $`6648`
## <PostgreSQLResult>
## 
## $`6649`
## <PostgreSQLResult>
## 
## $`6650`
## <PostgreSQLResult>
## 
## $`6651`
## <PostgreSQLResult>
## 
## $`6652`
## <PostgreSQLResult>
## 
## $`6653`
## <PostgreSQLResult>
## 
## $`6654`
## <PostgreSQLResult>
## 
## $`6655`
## <PostgreSQLResult>
## 
## $`6656`
## <PostgreSQLResult>
## 
## $`6657`
## <PostgreSQLResult>
## 
## $`6658`
## <PostgreSQLResult>
## 
## $`6659`
## <PostgreSQLResult>
## 
## $`6660`
## <PostgreSQLResult>
## 
## $`6661`
## <PostgreSQLResult>
## 
## $`6662`
## <PostgreSQLResult>
## 
## $`6663`
## <PostgreSQLResult>
## 
## $`6664`
## <PostgreSQLResult>
## 
## $`6665`
## <PostgreSQLResult>
## 
## $`6666`
## <PostgreSQLResult>
## 
## $`6709`
## <PostgreSQLResult>
## 
## $`6723`
## <PostgreSQLResult>
## 
## $`6750`
## <PostgreSQLResult>
## 
## $`6751`
## <PostgreSQLResult>
## 
## $`6752`
## <PostgreSQLResult>
## 
## $`6753`
## <PostgreSQLResult>
## 
## $`6754`
## <PostgreSQLResult>
## 
## $`6756`
## <PostgreSQLResult>
## 
## $`6757`
## <PostgreSQLResult>
## 
## $`6759`
## <PostgreSQLResult>
## 
## $`6761`
## <PostgreSQLResult>
## 
## $`6762`
## <PostgreSQLResult>
## 
## $`6763`
## <PostgreSQLResult>
## 
## $`6764`
## <PostgreSQLResult>
## 
## $`6766`
## <PostgreSQLResult>
## 
## $`6768`
## <PostgreSQLResult>
## 
## $`6769`
## <PostgreSQLResult>
## 
## $`6770`
## <PostgreSQLResult>
## 
## $`6771`
## <PostgreSQLResult>
## 
## $`6773`
## <PostgreSQLResult>
## 
## $`6774`
## <PostgreSQLResult>
## 
## $`6775`
## <PostgreSQLResult>
## 
## $`6776`
## <PostgreSQLResult>
## 
## $`6777`
## <PostgreSQLResult>
## 
## $`6778`
## <PostgreSQLResult>
## 
## $`6779`
## <PostgreSQLResult>
## 
## $`6780`
## <PostgreSQLResult>
## 
## $`6783`
## <PostgreSQLResult>
## 
## $`6786`
## <PostgreSQLResult>
## 
## $`6787`
## <PostgreSQLResult>
## 
## $`6789`
## <PostgreSQLResult>
## 
## $`6790`
## <PostgreSQLResult>
## 
## $`6791`
## <PostgreSQLResult>
## 
## $`6792`
## <PostgreSQLResult>
## 
## $`6793`
## <PostgreSQLResult>
## 
## $`6794`
## <PostgreSQLResult>
## 
## $`6795`
## <PostgreSQLResult>
## 
## $`6796`
## <PostgreSQLResult>
## 
## $`6797`
## <PostgreSQLResult>
## 
## $`6799`
## <PostgreSQLResult>
## 
## $`6801`
## <PostgreSQLResult>
## 
## $`6802`
## <PostgreSQLResult>
## 
## $`6803`
## <PostgreSQLResult>
## 
## $`6804`
## <PostgreSQLResult>
## 
## $`6805`
## <PostgreSQLResult>
## 
## $`6806`
## <PostgreSQLResult>
## 
## $`6807`
## <PostgreSQLResult>
## 
## $`6808`
## <PostgreSQLResult>
## 
## $`6809`
## <PostgreSQLResult>
## 
## $`6810`
## <PostgreSQLResult>
## 
## $`6811`
## <PostgreSQLResult>
## 
## $`6814`
## <PostgreSQLResult>
## 
## $`6815`
## <PostgreSQLResult>
## 
## $`6816`
## <PostgreSQLResult>
## 
## $`6817`
## <PostgreSQLResult>
## 
## $`6818`
## <PostgreSQLResult>
## 
## $`6819`
## <PostgreSQLResult>
## 
## $`6820`
## <PostgreSQLResult>
## 
## $`6821`
## <PostgreSQLResult>
## 
## $`6822`
## <PostgreSQLResult>
## 
## $`6823`
## <PostgreSQLResult>
## 
## $`6824`
## <PostgreSQLResult>
## 
## $`6825`
## <PostgreSQLResult>
## 
## $`6826`
## <PostgreSQLResult>
## 
## $`6827`
## <PostgreSQLResult>
## 
## $`6829`
## <PostgreSQLResult>
## 
## $`6830`
## <PostgreSQLResult>
## 
## $`6831`
## <PostgreSQLResult>
## 
## $`6832`
## <PostgreSQLResult>
## 
## $`6833`
## <PostgreSQLResult>
## 
## $`6834`
## <PostgreSQLResult>
## 
## $`6836`
## <PostgreSQLResult>
## 
## $`6837`
## <PostgreSQLResult>
## 
## $`6838`
## <PostgreSQLResult>
## 
## $`6839`
## <PostgreSQLResult>
## 
## $`6840`
## <PostgreSQLResult>
## 
## $`6842`
## <PostgreSQLResult>
## 
## $`6843`
## <PostgreSQLResult>
## 
## $`6844`
## <PostgreSQLResult>
## 
## $`6845`
## <PostgreSQLResult>
```

```r
na1 <- nrow(user.outlier)
# Outliers identified:
na1
```

```
## [1] 1239
```

```r
# Propotion (%) of outliers:
round(na1/sum(!is.na(user.continent_score$idi)) * 100, 1)
```

```
## [1] 18.1
```

Total outliers: 1239 out of 6846


