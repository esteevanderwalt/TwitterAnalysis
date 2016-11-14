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

###Score the data

##first create a score

```r
exp_no <- 1
period_no <- 1
factor_no <- 5

sql <- paste("DELETE FROM main.experiment_user_score where factor_no = 5", sep = "")
dbSendQuery(con, sql)
```

```
## <PostgreSQLResult>
```

```r
sql <- paste("INSERT INTO main.experiment_user_score(experiment_no, period_no, userid, factor_no, idi_full)", 
    sep = "")
sql <- paste(sql, "select experiment_no, period_no, userid, 5,", sep = "")
sql <- paste(sql, "sum((coalesce(dist_latlon_vs_tz,0)+coalesce(dist_latlon_vs_loc,0)+coalesce(dist_tz_vs_loc,0)))/sum(case when dist_latlon_vs_tz is null and dist_latlon_vs_loc is null and dist_tz_vs_loc is null then 1 else 0 end + case when dist_latlon_vs_tz is not null then 1 else 0 end + case when dist_latlon_vs_loc is not null then 1 else 0 end + case when dist_tz_vs_loc is not null then 1 else 0 end)", 
    sep = "")
sql <- paste(sql, " from main.experiment_user", sep = "")
sql <- paste(sql, " group by experiment_no, period_no, userid", sep = "")
dbSendQuery(con, sql)
```

```
## <PostgreSQLResult>
```

##normalise the score and show results
The standard formula to normalize data =(value-min)/(max-min) 
R has a built in function to do this

```r
user.score <- dbGetQuery(con, "SELECT s.idi_full, tz.continent, s.userid from main.experiment_user_score s join main.experiment_user u on u.userid = s.userid left join main.timezone_r tz on tz.timezone = u.timezone where s.factor_no = 5 and s.experiment_no = u.experiment_no and s.period_no = u.period_no")

user.scaled_score <- data.frame(as.data.frame(scale(user.score[1])), user.score[2], 
    user.score[3])
colnames(user.scaled_score) = c("idi", "continent", "userid")

ggplot(user.scaled_score, aes(x = continent, y = idi)) + geom_boxplot(outlier.colour = "red", 
    outlier.shape = 8, outlier.size = 4) + stat_summary(fun.y = mean, geom = "point", 
    shape = 23, size = 4)
```

![](FactorAnalysis-Distances_files/figure-html/score_n-1.png)<!-- -->


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
continents <- unique(user.scaled_score$continent)

user.continent_score <- user.scaled_score
outlier <- boxplot.stats(user.continent_score$idi, coef = 1.5)$out
user.outlier <- user.continent_score[user.continent_score$idi %in% outlier, 
    ]
apply(user.outlier, 1, markoutlier, exp_no = exp_no, period_no = period_no, 
    factor_no = factor_no)
```

```
## $`14`
## <PostgreSQLResult>
## 
## $`16`
## <PostgreSQLResult>
## 
## $`20`
## <PostgreSQLResult>
## 
## $`21`
## <PostgreSQLResult>
## 
## $`31`
## <PostgreSQLResult>
## 
## $`34`
## <PostgreSQLResult>
## 
## $`36`
## <PostgreSQLResult>
## 
## $`42`
## <PostgreSQLResult>
## 
## $`49`
## <PostgreSQLResult>
## 
## $`51`
## <PostgreSQLResult>
## 
## $`54`
## <PostgreSQLResult>
## 
## $`62`
## <PostgreSQLResult>
## 
## $`70`
## <PostgreSQLResult>
## 
## $`88`
## <PostgreSQLResult>
## 
## $`90`
## <PostgreSQLResult>
## 
## $`95`
## <PostgreSQLResult>
## 
## $`97`
## <PostgreSQLResult>
## 
## $`101`
## <PostgreSQLResult>
## 
## $`102`
## <PostgreSQLResult>
## 
## $`103`
## <PostgreSQLResult>
## 
## $`109`
## <PostgreSQLResult>
## 
## $`112`
## <PostgreSQLResult>
## 
## $`114`
## <PostgreSQLResult>
## 
## $`121`
## <PostgreSQLResult>
## 
## $`124`
## <PostgreSQLResult>
## 
## $`125`
## <PostgreSQLResult>
## 
## $`130`
## <PostgreSQLResult>
## 
## $`142`
## <PostgreSQLResult>
## 
## $`146`
## <PostgreSQLResult>
## 
## $`164`
## <PostgreSQLResult>
## 
## $`173`
## <PostgreSQLResult>
## 
## $`176`
## <PostgreSQLResult>
## 
## $`180`
## <PostgreSQLResult>
## 
## $`185`
## <PostgreSQLResult>
## 
## $`188`
## <PostgreSQLResult>
## 
## $`201`
## <PostgreSQLResult>
## 
## $`209`
## <PostgreSQLResult>
## 
## $`218`
## <PostgreSQLResult>
## 
## $`225`
## <PostgreSQLResult>
## 
## $`227`
## <PostgreSQLResult>
## 
## $`228`
## <PostgreSQLResult>
## 
## $`232`
## <PostgreSQLResult>
## 
## $`235`
## <PostgreSQLResult>
## 
## $`239`
## <PostgreSQLResult>
## 
## $`244`
## <PostgreSQLResult>
## 
## $`246`
## <PostgreSQLResult>
## 
## $`249`
## <PostgreSQLResult>
## 
## $`250`
## <PostgreSQLResult>
## 
## $`253`
## <PostgreSQLResult>
## 
## $`266`
## <PostgreSQLResult>
## 
## $`267`
## <PostgreSQLResult>
## 
## $`271`
## <PostgreSQLResult>
## 
## $`273`
## <PostgreSQLResult>
## 
## $`276`
## <PostgreSQLResult>
## 
## $`281`
## <PostgreSQLResult>
## 
## $`288`
## <PostgreSQLResult>
## 
## $`289`
## <PostgreSQLResult>
## 
## $`290`
## <PostgreSQLResult>
## 
## $`305`
## <PostgreSQLResult>
## 
## $`310`
## <PostgreSQLResult>
## 
## $`313`
## <PostgreSQLResult>
## 
## $`314`
## <PostgreSQLResult>
## 
## $`317`
## <PostgreSQLResult>
## 
## $`320`
## <PostgreSQLResult>
## 
## $`321`
## <PostgreSQLResult>
## 
## $`328`
## <PostgreSQLResult>
## 
## $`336`
## <PostgreSQLResult>
## 
## $`353`
## <PostgreSQLResult>
## 
## $`354`
## <PostgreSQLResult>
## 
## $`362`
## <PostgreSQLResult>
## 
## $`366`
## <PostgreSQLResult>
## 
## $`377`
## <PostgreSQLResult>
## 
## $`379`
## <PostgreSQLResult>
## 
## $`386`
## <PostgreSQLResult>
## 
## $`389`
## <PostgreSQLResult>
## 
## $`393`
## <PostgreSQLResult>
## 
## $`395`
## <PostgreSQLResult>
## 
## $`398`
## <PostgreSQLResult>
## 
## $`399`
## <PostgreSQLResult>
## 
## $`401`
## <PostgreSQLResult>
## 
## $`424`
## <PostgreSQLResult>
## 
## $`425`
## <PostgreSQLResult>
## 
## $`426`
## <PostgreSQLResult>
## 
## $`428`
## <PostgreSQLResult>
## 
## $`429`
## <PostgreSQLResult>
## 
## $`430`
## <PostgreSQLResult>
## 
## $`431`
## <PostgreSQLResult>
## 
## $`435`
## <PostgreSQLResult>
## 
## $`440`
## <PostgreSQLResult>
## 
## $`452`
## <PostgreSQLResult>
## 
## $`454`
## <PostgreSQLResult>
## 
## $`457`
## <PostgreSQLResult>
## 
## $`460`
## <PostgreSQLResult>
## 
## $`463`
## <PostgreSQLResult>
## 
## $`465`
## <PostgreSQLResult>
## 
## $`467`
## <PostgreSQLResult>
## 
## $`468`
## <PostgreSQLResult>
## 
## $`472`
## <PostgreSQLResult>
## 
## $`478`
## <PostgreSQLResult>
## 
## $`480`
## <PostgreSQLResult>
## 
## $`485`
## <PostgreSQLResult>
## 
## $`486`
## <PostgreSQLResult>
## 
## $`492`
## <PostgreSQLResult>
## 
## $`497`
## <PostgreSQLResult>
## 
## $`503`
## <PostgreSQLResult>
## 
## $`504`
## <PostgreSQLResult>
## 
## $`507`
## <PostgreSQLResult>
## 
## $`514`
## <PostgreSQLResult>
## 
## $`518`
## <PostgreSQLResult>
## 
## $`520`
## <PostgreSQLResult>
## 
## $`530`
## <PostgreSQLResult>
## 
## $`537`
## <PostgreSQLResult>
## 
## $`541`
## <PostgreSQLResult>
## 
## $`555`
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
## $`571`
## <PostgreSQLResult>
## 
## $`577`
## <PostgreSQLResult>
## 
## $`591`
## <PostgreSQLResult>
## 
## $`592`
## <PostgreSQLResult>
## 
## $`596`
## <PostgreSQLResult>
## 
## $`599`
## <PostgreSQLResult>
## 
## $`600`
## <PostgreSQLResult>
## 
## $`614`
## <PostgreSQLResult>
## 
## $`620`
## <PostgreSQLResult>
## 
## $`640`
## <PostgreSQLResult>
## 
## $`642`
## <PostgreSQLResult>
## 
## $`649`
## <PostgreSQLResult>
## 
## $`651`
## <PostgreSQLResult>
## 
## $`654`
## <PostgreSQLResult>
## 
## $`657`
## <PostgreSQLResult>
## 
## $`659`
## <PostgreSQLResult>
## 
## $`669`
## <PostgreSQLResult>
## 
## $`674`
## <PostgreSQLResult>
## 
## $`680`
## <PostgreSQLResult>
## 
## $`681`
## <PostgreSQLResult>
## 
## $`684`
## <PostgreSQLResult>
## 
## $`710`
## <PostgreSQLResult>
## 
## $`715`
## <PostgreSQLResult>
## 
## $`721`
## <PostgreSQLResult>
## 
## $`725`
## <PostgreSQLResult>
## 
## $`726`
## <PostgreSQLResult>
## 
## $`737`
## <PostgreSQLResult>
## 
## $`740`
## <PostgreSQLResult>
## 
## $`741`
## <PostgreSQLResult>
## 
## $`744`
## <PostgreSQLResult>
## 
## $`749`
## <PostgreSQLResult>
## 
## $`752`
## <PostgreSQLResult>
## 
## $`755`
## <PostgreSQLResult>
## 
## $`758`
## <PostgreSQLResult>
## 
## $`766`
## <PostgreSQLResult>
## 
## $`768`
## <PostgreSQLResult>
## 
## $`775`
## <PostgreSQLResult>
## 
## $`785`
## <PostgreSQLResult>
## 
## $`786`
## <PostgreSQLResult>
## 
## $`789`
## <PostgreSQLResult>
## 
## $`791`
## <PostgreSQLResult>
## 
## $`796`
## <PostgreSQLResult>
## 
## $`799`
## <PostgreSQLResult>
## 
## $`801`
## <PostgreSQLResult>
## 
## $`803`
## <PostgreSQLResult>
## 
## $`804`
## <PostgreSQLResult>
## 
## $`810`
## <PostgreSQLResult>
## 
## $`811`
## <PostgreSQLResult>
## 
## $`818`
## <PostgreSQLResult>
## 
## $`823`
## <PostgreSQLResult>
## 
## $`835`
## <PostgreSQLResult>
## 
## $`838`
## <PostgreSQLResult>
## 
## $`840`
## <PostgreSQLResult>
## 
## $`847`
## <PostgreSQLResult>
## 
## $`864`
## <PostgreSQLResult>
## 
## $`867`
## <PostgreSQLResult>
## 
## $`871`
## <PostgreSQLResult>
## 
## $`873`
## <PostgreSQLResult>
## 
## $`878`
## <PostgreSQLResult>
## 
## $`880`
## <PostgreSQLResult>
## 
## $`881`
## <PostgreSQLResult>
## 
## $`894`
## <PostgreSQLResult>
## 
## $`895`
## <PostgreSQLResult>
## 
## $`906`
## <PostgreSQLResult>
## 
## $`907`
## <PostgreSQLResult>
## 
## $`914`
## <PostgreSQLResult>
## 
## $`919`
## <PostgreSQLResult>
## 
## $`921`
## <PostgreSQLResult>
## 
## $`923`
## <PostgreSQLResult>
## 
## $`925`
## <PostgreSQLResult>
## 
## $`927`
## <PostgreSQLResult>
## 
## $`930`
## <PostgreSQLResult>
## 
## $`936`
## <PostgreSQLResult>
## 
## $`937`
## <PostgreSQLResult>
## 
## $`940`
## <PostgreSQLResult>
## 
## $`946`
## <PostgreSQLResult>
## 
## $`947`
## <PostgreSQLResult>
## 
## $`953`
## <PostgreSQLResult>
## 
## $`954`
## <PostgreSQLResult>
## 
## $`958`
## <PostgreSQLResult>
## 
## $`967`
## <PostgreSQLResult>
## 
## $`973`
## <PostgreSQLResult>
## 
## $`974`
## <PostgreSQLResult>
## 
## $`977`
## <PostgreSQLResult>
## 
## $`987`
## <PostgreSQLResult>
## 
## $`1010`
## <PostgreSQLResult>
## 
## $`1013`
## <PostgreSQLResult>
## 
## $`1018`
## <PostgreSQLResult>
## 
## $`1020`
## <PostgreSQLResult>
## 
## $`1028`
## <PostgreSQLResult>
## 
## $`1037`
## <PostgreSQLResult>
## 
## $`1040`
## <PostgreSQLResult>
## 
## $`1041`
## <PostgreSQLResult>
## 
## $`1043`
## <PostgreSQLResult>
## 
## $`1049`
## <PostgreSQLResult>
## 
## $`1054`
## <PostgreSQLResult>
## 
## $`1055`
## <PostgreSQLResult>
## 
## $`1058`
## <PostgreSQLResult>
## 
## $`1059`
## <PostgreSQLResult>
## 
## $`1062`
## <PostgreSQLResult>
## 
## $`1069`
## <PostgreSQLResult>
## 
## $`1072`
## <PostgreSQLResult>
## 
## $`1075`
## <PostgreSQLResult>
## 
## $`1076`
## <PostgreSQLResult>
## 
## $`1077`
## <PostgreSQLResult>
## 
## $`1078`
## <PostgreSQLResult>
## 
## $`1088`
## <PostgreSQLResult>
## 
## $`1095`
## <PostgreSQLResult>
## 
## $`1099`
## <PostgreSQLResult>
## 
## $`1101`
## <PostgreSQLResult>
## 
## $`1104`
## <PostgreSQLResult>
## 
## $`1106`
## <PostgreSQLResult>
## 
## $`1107`
## <PostgreSQLResult>
## 
## $`1108`
## <PostgreSQLResult>
## 
## $`1110`
## <PostgreSQLResult>
## 
## $`1116`
## <PostgreSQLResult>
## 
## $`1117`
## <PostgreSQLResult>
## 
## $`1118`
## <PostgreSQLResult>
## 
## $`1121`
## <PostgreSQLResult>
## 
## $`1125`
## <PostgreSQLResult>
## 
## $`1127`
## <PostgreSQLResult>
## 
## $`1133`
## <PostgreSQLResult>
## 
## $`1136`
## <PostgreSQLResult>
## 
## $`1140`
## <PostgreSQLResult>
## 
## $`1144`
## <PostgreSQLResult>
## 
## $`1149`
## <PostgreSQLResult>
## 
## $`1151`
## <PostgreSQLResult>
## 
## $`1170`
## <PostgreSQLResult>
## 
## $`1174`
## <PostgreSQLResult>
## 
## $`1179`
## <PostgreSQLResult>
## 
## $`1182`
## <PostgreSQLResult>
## 
## $`1185`
## <PostgreSQLResult>
## 
## $`1187`
## <PostgreSQLResult>
## 
## $`1190`
## <PostgreSQLResult>
## 
## $`1191`
## <PostgreSQLResult>
## 
## $`1192`
## <PostgreSQLResult>
## 
## $`1197`
## <PostgreSQLResult>
## 
## $`1204`
## <PostgreSQLResult>
## 
## $`1211`
## <PostgreSQLResult>
## 
## $`1212`
## <PostgreSQLResult>
## 
## $`1213`
## <PostgreSQLResult>
## 
## $`1217`
## <PostgreSQLResult>
## 
## $`1219`
## <PostgreSQLResult>
## 
## $`1220`
## <PostgreSQLResult>
## 
## $`1227`
## <PostgreSQLResult>
## 
## $`1228`
## <PostgreSQLResult>
## 
## $`1233`
## <PostgreSQLResult>
## 
## $`1234`
## <PostgreSQLResult>
## 
## $`1235`
## <PostgreSQLResult>
## 
## $`1237`
## <PostgreSQLResult>
## 
## $`1241`
## <PostgreSQLResult>
## 
## $`1244`
## <PostgreSQLResult>
## 
## $`1247`
## <PostgreSQLResult>
## 
## $`1248`
## <PostgreSQLResult>
## 
## $`1251`
## <PostgreSQLResult>
## 
## $`1260`
## <PostgreSQLResult>
## 
## $`1261`
## <PostgreSQLResult>
## 
## $`1263`
## <PostgreSQLResult>
## 
## $`1264`
## <PostgreSQLResult>
## 
## $`1272`
## <PostgreSQLResult>
## 
## $`1277`
## <PostgreSQLResult>
## 
## $`1279`
## <PostgreSQLResult>
## 
## $`1284`
## <PostgreSQLResult>
## 
## $`1285`
## <PostgreSQLResult>
## 
## $`1287`
## <PostgreSQLResult>
## 
## $`1295`
## <PostgreSQLResult>
## 
## $`1298`
## <PostgreSQLResult>
## 
## $`1302`
## <PostgreSQLResult>
## 
## $`1305`
## <PostgreSQLResult>
## 
## $`1319`
## <PostgreSQLResult>
## 
## $`1332`
## <PostgreSQLResult>
## 
## $`1333`
## <PostgreSQLResult>
## 
## $`1337`
## <PostgreSQLResult>
## 
## $`1338`
## <PostgreSQLResult>
## 
## $`1341`
## <PostgreSQLResult>
## 
## $`1344`
## <PostgreSQLResult>
## 
## $`1353`
## <PostgreSQLResult>
## 
## $`1355`
## <PostgreSQLResult>
## 
## $`1357`
## <PostgreSQLResult>
## 
## $`1360`
## <PostgreSQLResult>
## 
## $`1361`
## <PostgreSQLResult>
## 
## $`1364`
## <PostgreSQLResult>
## 
## $`1366`
## <PostgreSQLResult>
## 
## $`1368`
## <PostgreSQLResult>
## 
## $`1376`
## <PostgreSQLResult>
## 
## $`1384`
## <PostgreSQLResult>
## 
## $`1391`
## <PostgreSQLResult>
## 
## $`1400`
## <PostgreSQLResult>
## 
## $`1401`
## <PostgreSQLResult>
## 
## $`1406`
## <PostgreSQLResult>
## 
## $`1409`
## <PostgreSQLResult>
## 
## $`1410`
## <PostgreSQLResult>
## 
## $`1411`
## <PostgreSQLResult>
## 
## $`1420`
## <PostgreSQLResult>
## 
## $`1423`
## <PostgreSQLResult>
## 
## $`1424`
## <PostgreSQLResult>
## 
## $`1426`
## <PostgreSQLResult>
## 
## $`1428`
## <PostgreSQLResult>
## 
## $`1436`
## <PostgreSQLResult>
## 
## $`1444`
## <PostgreSQLResult>
## 
## $`1445`
## <PostgreSQLResult>
## 
## $`1446`
## <PostgreSQLResult>
## 
## $`1456`
## <PostgreSQLResult>
## 
## $`1462`
## <PostgreSQLResult>
## 
## $`1469`
## <PostgreSQLResult>
## 
## $`1471`
## <PostgreSQLResult>
## 
## $`1473`
## <PostgreSQLResult>
## 
## $`1478`
## <PostgreSQLResult>
## 
## $`1490`
## <PostgreSQLResult>
## 
## $`1493`
## <PostgreSQLResult>
## 
## $`1501`
## <PostgreSQLResult>
## 
## $`1506`
## <PostgreSQLResult>
## 
## $`1509`
## <PostgreSQLResult>
## 
## $`1510`
## <PostgreSQLResult>
## 
## $`1521`
## <PostgreSQLResult>
## 
## $`1528`
## <PostgreSQLResult>
## 
## $`1538`
## <PostgreSQLResult>
## 
## $`1547`
## <PostgreSQLResult>
## 
## $`1549`
## <PostgreSQLResult>
## 
## $`1554`
## <PostgreSQLResult>
## 
## $`1558`
## <PostgreSQLResult>
## 
## $`1559`
## <PostgreSQLResult>
## 
## $`1564`
## <PostgreSQLResult>
## 
## $`1565`
## <PostgreSQLResult>
## 
## $`1570`
## <PostgreSQLResult>
## 
## $`1574`
## <PostgreSQLResult>
## 
## $`1575`
## <PostgreSQLResult>
## 
## $`1579`
## <PostgreSQLResult>
## 
## $`1587`
## <PostgreSQLResult>
## 
## $`1592`
## <PostgreSQLResult>
## 
## $`1597`
## <PostgreSQLResult>
## 
## $`1598`
## <PostgreSQLResult>
## 
## $`1599`
## <PostgreSQLResult>
## 
## $`1602`
## <PostgreSQLResult>
## 
## $`1613`
## <PostgreSQLResult>
## 
## $`1633`
## <PostgreSQLResult>
## 
## $`1634`
## <PostgreSQLResult>
## 
## $`1637`
## <PostgreSQLResult>
## 
## $`1638`
## <PostgreSQLResult>
## 
## $`1639`
## <PostgreSQLResult>
## 
## $`1641`
## <PostgreSQLResult>
## 
## $`1644`
## <PostgreSQLResult>
## 
## $`1645`
## <PostgreSQLResult>
## 
## $`1657`
## <PostgreSQLResult>
## 
## $`1659`
## <PostgreSQLResult>
## 
## $`1664`
## <PostgreSQLResult>
## 
## $`1665`
## <PostgreSQLResult>
## 
## $`1667`
## <PostgreSQLResult>
## 
## $`1668`
## <PostgreSQLResult>
## 
## $`1671`
## <PostgreSQLResult>
## 
## $`1679`
## <PostgreSQLResult>
## 
## $`1685`
## <PostgreSQLResult>
## 
## $`1689`
## <PostgreSQLResult>
## 
## $`1693`
## <PostgreSQLResult>
## 
## $`1697`
## <PostgreSQLResult>
## 
## $`1704`
## <PostgreSQLResult>
## 
## $`1706`
## <PostgreSQLResult>
## 
## $`1707`
## <PostgreSQLResult>
## 
## $`1709`
## <PostgreSQLResult>
## 
## $`1719`
## <PostgreSQLResult>
## 
## $`1724`
## <PostgreSQLResult>
## 
## $`1726`
## <PostgreSQLResult>
## 
## $`1738`
## <PostgreSQLResult>
## 
## $`1741`
## <PostgreSQLResult>
## 
## $`1742`
## <PostgreSQLResult>
## 
## $`1769`
## <PostgreSQLResult>
## 
## $`1770`
## <PostgreSQLResult>
## 
## $`1772`
## <PostgreSQLResult>
## 
## $`1778`
## <PostgreSQLResult>
## 
## $`1784`
## <PostgreSQLResult>
## 
## $`1786`
## <PostgreSQLResult>
## 
## $`1788`
## <PostgreSQLResult>
## 
## $`1789`
## <PostgreSQLResult>
## 
## $`1793`
## <PostgreSQLResult>
## 
## $`1794`
## <PostgreSQLResult>
## 
## $`1797`
## <PostgreSQLResult>
## 
## $`1821`
## <PostgreSQLResult>
## 
## $`1822`
## <PostgreSQLResult>
## 
## $`1825`
## <PostgreSQLResult>
## 
## $`1827`
## <PostgreSQLResult>
## 
## $`1832`
## <PostgreSQLResult>
## 
## $`1835`
## <PostgreSQLResult>
## 
## $`1836`
## <PostgreSQLResult>
## 
## $`1840`
## <PostgreSQLResult>
## 
## $`1848`
## <PostgreSQLResult>
## 
## $`1862`
## <PostgreSQLResult>
## 
## $`1863`
## <PostgreSQLResult>
## 
## $`1869`
## <PostgreSQLResult>
## 
## $`1870`
## <PostgreSQLResult>
## 
## $`1881`
## <PostgreSQLResult>
## 
## $`1888`
## <PostgreSQLResult>
## 
## $`1890`
## <PostgreSQLResult>
## 
## $`1892`
## <PostgreSQLResult>
## 
## $`1896`
## <PostgreSQLResult>
## 
## $`1898`
## <PostgreSQLResult>
## 
## $`1899`
## <PostgreSQLResult>
## 
## $`1902`
## <PostgreSQLResult>
## 
## $`1912`
## <PostgreSQLResult>
## 
## $`1919`
## <PostgreSQLResult>
## 
## $`1929`
## <PostgreSQLResult>
## 
## $`1932`
## <PostgreSQLResult>
## 
## $`1934`
## <PostgreSQLResult>
## 
## $`1935`
## <PostgreSQLResult>
## 
## $`1937`
## <PostgreSQLResult>
## 
## $`1939`
## <PostgreSQLResult>
## 
## $`1940`
## <PostgreSQLResult>
## 
## $`1949`
## <PostgreSQLResult>
## 
## $`1950`
## <PostgreSQLResult>
## 
## $`1952`
## <PostgreSQLResult>
## 
## $`1955`
## <PostgreSQLResult>
## 
## $`1957`
## <PostgreSQLResult>
## 
## $`1959`
## <PostgreSQLResult>
## 
## $`1960`
## <PostgreSQLResult>
## 
## $`1963`
## <PostgreSQLResult>
## 
## $`1971`
## <PostgreSQLResult>
## 
## $`1973`
## <PostgreSQLResult>
## 
## $`1980`
## <PostgreSQLResult>
## 
## $`1982`
## <PostgreSQLResult>
## 
## $`1985`
## <PostgreSQLResult>
## 
## $`1988`
## <PostgreSQLResult>
## 
## $`1996`
## <PostgreSQLResult>
## 
## $`2000`
## <PostgreSQLResult>
## 
## $`2002`
## <PostgreSQLResult>
## 
## $`2008`
## <PostgreSQLResult>
## 
## $`2012`
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
## $`2020`
## <PostgreSQLResult>
## 
## $`2023`
## <PostgreSQLResult>
## 
## $`2032`
## <PostgreSQLResult>
## 
## $`2044`
## <PostgreSQLResult>
## 
## $`2046`
## <PostgreSQLResult>
## 
## $`2050`
## <PostgreSQLResult>
## 
## $`2053`
## <PostgreSQLResult>
## 
## $`2055`
## <PostgreSQLResult>
## 
## $`2063`
## <PostgreSQLResult>
## 
## $`2064`
## <PostgreSQLResult>
## 
## $`2065`
## <PostgreSQLResult>
## 
## $`2072`
## <PostgreSQLResult>
## 
## $`2074`
## <PostgreSQLResult>
## 
## $`2076`
## <PostgreSQLResult>
## 
## $`2081`
## <PostgreSQLResult>
## 
## $`2095`
## <PostgreSQLResult>
## 
## $`2098`
## <PostgreSQLResult>
## 
## $`2103`
## <PostgreSQLResult>
## 
## $`2110`
## <PostgreSQLResult>
## 
## $`2115`
## <PostgreSQLResult>
## 
## $`2122`
## <PostgreSQLResult>
## 
## $`2123`
## <PostgreSQLResult>
## 
## $`2129`
## <PostgreSQLResult>
## 
## $`2130`
## <PostgreSQLResult>
## 
## $`2146`
## <PostgreSQLResult>
## 
## $`2150`
## <PostgreSQLResult>
## 
## $`2159`
## <PostgreSQLResult>
## 
## $`2161`
## <PostgreSQLResult>
## 
## $`2162`
## <PostgreSQLResult>
## 
## $`2163`
## <PostgreSQLResult>
## 
## $`2167`
## <PostgreSQLResult>
## 
## $`2176`
## <PostgreSQLResult>
## 
## $`2179`
## <PostgreSQLResult>
## 
## $`2181`
## <PostgreSQLResult>
## 
## $`2187`
## <PostgreSQLResult>
## 
## $`2193`
## <PostgreSQLResult>
## 
## $`2196`
## <PostgreSQLResult>
## 
## $`2203`
## <PostgreSQLResult>
## 
## $`2208`
## <PostgreSQLResult>
## 
## $`2216`
## <PostgreSQLResult>
## 
## $`2217`
## <PostgreSQLResult>
## 
## $`2230`
## <PostgreSQLResult>
## 
## $`2231`
## <PostgreSQLResult>
## 
## $`2233`
## <PostgreSQLResult>
## 
## $`2245`
## <PostgreSQLResult>
## 
## $`2255`
## <PostgreSQLResult>
## 
## $`2256`
## <PostgreSQLResult>
## 
## $`2258`
## <PostgreSQLResult>
## 
## $`2266`
## <PostgreSQLResult>
## 
## $`2277`
## <PostgreSQLResult>
## 
## $`2279`
## <PostgreSQLResult>
## 
## $`2283`
## <PostgreSQLResult>
## 
## $`2293`
## <PostgreSQLResult>
## 
## $`2296`
## <PostgreSQLResult>
## 
## $`2297`
## <PostgreSQLResult>
## 
## $`2302`
## <PostgreSQLResult>
## 
## $`2310`
## <PostgreSQLResult>
## 
## $`2311`
## <PostgreSQLResult>
## 
## $`2316`
## <PostgreSQLResult>
## 
## $`2319`
## <PostgreSQLResult>
## 
## $`2322`
## <PostgreSQLResult>
## 
## $`2327`
## <PostgreSQLResult>
## 
## $`2331`
## <PostgreSQLResult>
## 
## $`2336`
## <PostgreSQLResult>
## 
## $`2344`
## <PostgreSQLResult>
## 
## $`2346`
## <PostgreSQLResult>
## 
## $`2358`
## <PostgreSQLResult>
## 
## $`2368`
## <PostgreSQLResult>
## 
## $`2372`
## <PostgreSQLResult>
## 
## $`2384`
## <PostgreSQLResult>
## 
## $`2388`
## <PostgreSQLResult>
## 
## $`2395`
## <PostgreSQLResult>
## 
## $`2399`
## <PostgreSQLResult>
## 
## $`2405`
## <PostgreSQLResult>
## 
## $`2414`
## <PostgreSQLResult>
## 
## $`2416`
## <PostgreSQLResult>
## 
## $`2417`
## <PostgreSQLResult>
## 
## $`2422`
## <PostgreSQLResult>
## 
## $`2425`
## <PostgreSQLResult>
## 
## $`2427`
## <PostgreSQLResult>
## 
## $`2430`
## <PostgreSQLResult>
## 
## $`2460`
## <PostgreSQLResult>
## 
## $`2462`
## <PostgreSQLResult>
## 
## $`2465`
## <PostgreSQLResult>
## 
## $`2468`
## <PostgreSQLResult>
## 
## $`2478`
## <PostgreSQLResult>
## 
## $`2479`
## <PostgreSQLResult>
## 
## $`2481`
## <PostgreSQLResult>
## 
## $`2483`
## <PostgreSQLResult>
## 
## $`2484`
## <PostgreSQLResult>
## 
## $`2490`
## <PostgreSQLResult>
## 
## $`2495`
## <PostgreSQLResult>
## 
## $`2501`
## <PostgreSQLResult>
## 
## $`2505`
## <PostgreSQLResult>
## 
## $`2508`
## <PostgreSQLResult>
## 
## $`2509`
## <PostgreSQLResult>
## 
## $`2512`
## <PostgreSQLResult>
## 
## $`2513`
## <PostgreSQLResult>
## 
## $`2517`
## <PostgreSQLResult>
## 
## $`2524`
## <PostgreSQLResult>
## 
## $`2526`
## <PostgreSQLResult>
## 
## $`2527`
## <PostgreSQLResult>
## 
## $`2533`
## <PostgreSQLResult>
## 
## $`2540`
## <PostgreSQLResult>
## 
## $`2543`
## <PostgreSQLResult>
## 
## $`2546`
## <PostgreSQLResult>
## 
## $`2549`
## <PostgreSQLResult>
## 
## $`2551`
## <PostgreSQLResult>
## 
## $`2553`
## <PostgreSQLResult>
## 
## $`2567`
## <PostgreSQLResult>
## 
## $`2568`
## <PostgreSQLResult>
## 
## $`2573`
## <PostgreSQLResult>
## 
## $`2576`
## <PostgreSQLResult>
## 
## $`2578`
## <PostgreSQLResult>
## 
## $`2585`
## <PostgreSQLResult>
## 
## $`2591`
## <PostgreSQLResult>
## 
## $`2592`
## <PostgreSQLResult>
## 
## $`2596`
## <PostgreSQLResult>
## 
## $`2598`
## <PostgreSQLResult>
## 
## $`2599`
## <PostgreSQLResult>
## 
## $`2601`
## <PostgreSQLResult>
## 
## $`2610`
## <PostgreSQLResult>
## 
## $`2613`
## <PostgreSQLResult>
## 
## $`2618`
## <PostgreSQLResult>
## 
## $`2620`
## <PostgreSQLResult>
## 
## $`2637`
## <PostgreSQLResult>
## 
## $`2638`
## <PostgreSQLResult>
## 
## $`2646`
## <PostgreSQLResult>
## 
## $`2647`
## <PostgreSQLResult>
## 
## $`2653`
## <PostgreSQLResult>
## 
## $`2659`
## <PostgreSQLResult>
## 
## $`2664`
## <PostgreSQLResult>
## 
## $`2667`
## <PostgreSQLResult>
## 
## $`2669`
## <PostgreSQLResult>
## 
## $`2678`
## <PostgreSQLResult>
## 
## $`2679`
## <PostgreSQLResult>
## 
## $`2680`
## <PostgreSQLResult>
## 
## $`2681`
## <PostgreSQLResult>
## 
## $`2683`
## <PostgreSQLResult>
## 
## $`2684`
## <PostgreSQLResult>
## 
## $`2688`
## <PostgreSQLResult>
## 
## $`2689`
## <PostgreSQLResult>
## 
## $`2697`
## <PostgreSQLResult>
## 
## $`2701`
## <PostgreSQLResult>
## 
## $`2702`
## <PostgreSQLResult>
## 
## $`2705`
## <PostgreSQLResult>
## 
## $`2713`
## <PostgreSQLResult>
## 
## $`2716`
## <PostgreSQLResult>
## 
## $`2728`
## <PostgreSQLResult>
## 
## $`2730`
## <PostgreSQLResult>
## 
## $`2736`
## <PostgreSQLResult>
## 
## $`2738`
## <PostgreSQLResult>
## 
## $`2742`
## <PostgreSQLResult>
## 
## $`2749`
## <PostgreSQLResult>
## 
## $`2755`
## <PostgreSQLResult>
## 
## $`2766`
## <PostgreSQLResult>
## 
## $`2770`
## <PostgreSQLResult>
## 
## $`2777`
## <PostgreSQLResult>
## 
## $`2779`
## <PostgreSQLResult>
## 
## $`2783`
## <PostgreSQLResult>
## 
## $`2785`
## <PostgreSQLResult>
## 
## $`2789`
## <PostgreSQLResult>
## 
## $`2796`
## <PostgreSQLResult>
## 
## $`2799`
## <PostgreSQLResult>
## 
## $`2800`
## <PostgreSQLResult>
## 
## $`2801`
## <PostgreSQLResult>
## 
## $`2810`
## <PostgreSQLResult>
## 
## $`2820`
## <PostgreSQLResult>
## 
## $`2822`
## <PostgreSQLResult>
## 
## $`2831`
## <PostgreSQLResult>
## 
## $`2833`
## <PostgreSQLResult>
## 
## $`2834`
## <PostgreSQLResult>
## 
## $`2840`
## <PostgreSQLResult>
## 
## $`2841`
## <PostgreSQLResult>
## 
## $`2849`
## <PostgreSQLResult>
## 
## $`2851`
## <PostgreSQLResult>
## 
## $`2853`
## <PostgreSQLResult>
## 
## $`2862`
## <PostgreSQLResult>
## 
## $`2864`
## <PostgreSQLResult>
## 
## $`2867`
## <PostgreSQLResult>
## 
## $`2872`
## <PostgreSQLResult>
## 
## $`2877`
## <PostgreSQLResult>
## 
## $`2878`
## <PostgreSQLResult>
## 
## $`2881`
## <PostgreSQLResult>
## 
## $`2890`
## <PostgreSQLResult>
## 
## $`2896`
## <PostgreSQLResult>
## 
## $`2899`
## <PostgreSQLResult>
## 
## $`2918`
## <PostgreSQLResult>
## 
## $`2922`
## <PostgreSQLResult>
## 
## $`2923`
## <PostgreSQLResult>
## 
## $`2934`
## <PostgreSQLResult>
## 
## $`2944`
## <PostgreSQLResult>
## 
## $`2955`
## <PostgreSQLResult>
## 
## $`2956`
## <PostgreSQLResult>
## 
## $`2962`
## <PostgreSQLResult>
## 
## $`2967`
## <PostgreSQLResult>
## 
## $`2968`
## <PostgreSQLResult>
## 
## $`2977`
## <PostgreSQLResult>
## 
## $`2978`
## <PostgreSQLResult>
## 
## $`2979`
## <PostgreSQLResult>
## 
## $`2984`
## <PostgreSQLResult>
## 
## $`2990`
## <PostgreSQLResult>
## 
## $`2995`
## <PostgreSQLResult>
## 
## $`2999`
## <PostgreSQLResult>
## 
## $`3001`
## <PostgreSQLResult>
## 
## $`3006`
## <PostgreSQLResult>
## 
## $`3013`
## <PostgreSQLResult>
## 
## $`3014`
## <PostgreSQLResult>
## 
## $`3016`
## <PostgreSQLResult>
## 
## $`3022`
## <PostgreSQLResult>
## 
## $`3028`
## <PostgreSQLResult>
## 
## $`3031`
## <PostgreSQLResult>
## 
## $`3034`
## <PostgreSQLResult>
## 
## $`3035`
## <PostgreSQLResult>
## 
## $`3046`
## <PostgreSQLResult>
## 
## $`3052`
## <PostgreSQLResult>
## 
## $`3053`
## <PostgreSQLResult>
## 
## $`3054`
## <PostgreSQLResult>
## 
## $`3055`
## <PostgreSQLResult>
## 
## $`3057`
## <PostgreSQLResult>
## 
## $`3059`
## <PostgreSQLResult>
## 
## $`3065`
## <PostgreSQLResult>
## 
## $`3066`
## <PostgreSQLResult>
## 
## $`3067`
## <PostgreSQLResult>
## 
## $`3069`
## <PostgreSQLResult>
## 
## $`3077`
## <PostgreSQLResult>
## 
## $`3091`
## <PostgreSQLResult>
## 
## $`3095`
## <PostgreSQLResult>
## 
## $`3102`
## <PostgreSQLResult>
## 
## $`3106`
## <PostgreSQLResult>
## 
## $`3107`
## <PostgreSQLResult>
## 
## $`3115`
## <PostgreSQLResult>
## 
## $`3116`
## <PostgreSQLResult>
## 
## $`3118`
## <PostgreSQLResult>
## 
## $`3121`
## <PostgreSQLResult>
## 
## $`3125`
## <PostgreSQLResult>
## 
## $`3128`
## <PostgreSQLResult>
## 
## $`3132`
## <PostgreSQLResult>
## 
## $`3134`
## <PostgreSQLResult>
## 
## $`3136`
## <PostgreSQLResult>
## 
## $`3139`
## <PostgreSQLResult>
## 
## $`3143`
## <PostgreSQLResult>
## 
## $`3153`
## <PostgreSQLResult>
## 
## $`3158`
## <PostgreSQLResult>
## 
## $`3161`
## <PostgreSQLResult>
## 
## $`3164`
## <PostgreSQLResult>
## 
## $`3165`
## <PostgreSQLResult>
## 
## $`3172`
## <PostgreSQLResult>
## 
## $`3179`
## <PostgreSQLResult>
## 
## $`3181`
## <PostgreSQLResult>
## 
## $`3186`
## <PostgreSQLResult>
## 
## $`3188`
## <PostgreSQLResult>
## 
## $`3197`
## <PostgreSQLResult>
## 
## $`3208`
## <PostgreSQLResult>
## 
## $`3209`
## <PostgreSQLResult>
## 
## $`3214`
## <PostgreSQLResult>
## 
## $`3216`
## <PostgreSQLResult>
## 
## $`3221`
## <PostgreSQLResult>
## 
## $`3222`
## <PostgreSQLResult>
## 
## $`3224`
## <PostgreSQLResult>
## 
## $`3231`
## <PostgreSQLResult>
## 
## $`3235`
## <PostgreSQLResult>
## 
## $`3255`
## <PostgreSQLResult>
## 
## $`3259`
## <PostgreSQLResult>
## 
## $`3268`
## <PostgreSQLResult>
## 
## $`3277`
## <PostgreSQLResult>
## 
## $`3279`
## <PostgreSQLResult>
## 
## $`3287`
## <PostgreSQLResult>
## 
## $`3289`
## <PostgreSQLResult>
## 
## $`3291`
## <PostgreSQLResult>
## 
## $`3292`
## <PostgreSQLResult>
## 
## $`3306`
## <PostgreSQLResult>
## 
## $`3307`
## <PostgreSQLResult>
## 
## $`3310`
## <PostgreSQLResult>
## 
## $`3312`
## <PostgreSQLResult>
## 
## $`3318`
## <PostgreSQLResult>
## 
## $`3322`
## <PostgreSQLResult>
## 
## $`3325`
## <PostgreSQLResult>
## 
## $`3326`
## <PostgreSQLResult>
## 
## $`3333`
## <PostgreSQLResult>
## 
## $`3334`
## <PostgreSQLResult>
## 
## $`3337`
## <PostgreSQLResult>
## 
## $`3338`
## <PostgreSQLResult>
## 
## $`3340`
## <PostgreSQLResult>
## 
## $`3349`
## <PostgreSQLResult>
## 
## $`3351`
## <PostgreSQLResult>
## 
## $`3358`
## <PostgreSQLResult>
## 
## $`3362`
## <PostgreSQLResult>
## 
## $`3366`
## <PostgreSQLResult>
## 
## $`3381`
## <PostgreSQLResult>
## 
## $`3384`
## <PostgreSQLResult>
## 
## $`3386`
## <PostgreSQLResult>
## 
## $`3396`
## <PostgreSQLResult>
## 
## $`3398`
## <PostgreSQLResult>
## 
## $`3406`
## <PostgreSQLResult>
## 
## $`3424`
## <PostgreSQLResult>
## 
## $`3433`
## <PostgreSQLResult>
## 
## $`3434`
## <PostgreSQLResult>
## 
## $`3438`
## <PostgreSQLResult>
## 
## $`3443`
## <PostgreSQLResult>
## 
## $`3450`
## <PostgreSQLResult>
## 
## $`3452`
## <PostgreSQLResult>
## 
## $`3466`
## <PostgreSQLResult>
## 
## $`3469`
## <PostgreSQLResult>
## 
## $`3470`
## <PostgreSQLResult>
## 
## $`3471`
## <PostgreSQLResult>
## 
## $`3474`
## <PostgreSQLResult>
## 
## $`3477`
## <PostgreSQLResult>
## 
## $`3485`
## <PostgreSQLResult>
## 
## $`3486`
## <PostgreSQLResult>
## 
## $`3491`
## <PostgreSQLResult>
## 
## $`3498`
## <PostgreSQLResult>
## 
## $`3510`
## <PostgreSQLResult>
## 
## $`3511`
## <PostgreSQLResult>
## 
## $`3512`
## <PostgreSQLResult>
## 
## $`3513`
## <PostgreSQLResult>
## 
## $`3517`
## <PostgreSQLResult>
## 
## $`3518`
## <PostgreSQLResult>
## 
## $`3521`
## <PostgreSQLResult>
## 
## $`3525`
## <PostgreSQLResult>
## 
## $`3540`
## <PostgreSQLResult>
## 
## $`3541`
## <PostgreSQLResult>
## 
## $`3549`
## <PostgreSQLResult>
## 
## $`3560`
## <PostgreSQLResult>
## 
## $`3563`
## <PostgreSQLResult>
## 
## $`3566`
## <PostgreSQLResult>
## 
## $`3567`
## <PostgreSQLResult>
## 
## $`3569`
## <PostgreSQLResult>
## 
## $`3570`
## <PostgreSQLResult>
## 
## $`3578`
## <PostgreSQLResult>
## 
## $`3579`
## <PostgreSQLResult>
## 
## $`3580`
## <PostgreSQLResult>
## 
## $`3583`
## <PostgreSQLResult>
## 
## $`3592`
## <PostgreSQLResult>
## 
## $`3593`
## <PostgreSQLResult>
## 
## $`3596`
## <PostgreSQLResult>
## 
## $`3602`
## <PostgreSQLResult>
## 
## $`3604`
## <PostgreSQLResult>
## 
## $`3610`
## <PostgreSQLResult>
## 
## $`3619`
## <PostgreSQLResult>
## 
## $`3621`
## <PostgreSQLResult>
## 
## $`3622`
## <PostgreSQLResult>
## 
## $`3625`
## <PostgreSQLResult>
## 
## $`3632`
## <PostgreSQLResult>
## 
## $`3633`
## <PostgreSQLResult>
## 
## $`3636`
## <PostgreSQLResult>
## 
## $`3637`
## <PostgreSQLResult>
## 
## $`3638`
## <PostgreSQLResult>
## 
## $`3639`
## <PostgreSQLResult>
## 
## $`3644`
## <PostgreSQLResult>
## 
## $`3646`
## <PostgreSQLResult>
## 
## $`3649`
## <PostgreSQLResult>
## 
## $`3652`
## <PostgreSQLResult>
## 
## $`3664`
## <PostgreSQLResult>
## 
## $`3667`
## <PostgreSQLResult>
## 
## $`3672`
## <PostgreSQLResult>
## 
## $`3681`
## <PostgreSQLResult>
## 
## $`3686`
## <PostgreSQLResult>
## 
## $`3687`
## <PostgreSQLResult>
## 
## $`3692`
## <PostgreSQLResult>
## 
## $`3697`
## <PostgreSQLResult>
## 
## $`3698`
## <PostgreSQLResult>
## 
## $`3710`
## <PostgreSQLResult>
## 
## $`3718`
## <PostgreSQLResult>
## 
## $`3724`
## <PostgreSQLResult>
## 
## $`3730`
## <PostgreSQLResult>
## 
## $`3740`
## <PostgreSQLResult>
## 
## $`3742`
## <PostgreSQLResult>
## 
## $`3743`
## <PostgreSQLResult>
## 
## $`3750`
## <PostgreSQLResult>
## 
## $`3752`
## <PostgreSQLResult>
## 
## $`3758`
## <PostgreSQLResult>
## 
## $`3772`
## <PostgreSQLResult>
## 
## $`3775`
## <PostgreSQLResult>
## 
## $`3776`
## <PostgreSQLResult>
## 
## $`3781`
## <PostgreSQLResult>
## 
## $`3792`
## <PostgreSQLResult>
## 
## $`3796`
## <PostgreSQLResult>
## 
## $`3801`
## <PostgreSQLResult>
## 
## $`3802`
## <PostgreSQLResult>
## 
## $`3803`
## <PostgreSQLResult>
## 
## $`3808`
## <PostgreSQLResult>
## 
## $`3814`
## <PostgreSQLResult>
## 
## $`3819`
## <PostgreSQLResult>
## 
## $`3829`
## <PostgreSQLResult>
## 
## $`3832`
## <PostgreSQLResult>
## 
## $`3833`
## <PostgreSQLResult>
## 
## $`3834`
## <PostgreSQLResult>
## 
## $`3841`
## <PostgreSQLResult>
## 
## $`3846`
## <PostgreSQLResult>
## 
## $`3848`
## <PostgreSQLResult>
## 
## $`3852`
## <PostgreSQLResult>
## 
## $`3858`
## <PostgreSQLResult>
## 
## $`3861`
## <PostgreSQLResult>
## 
## $`3866`
## <PostgreSQLResult>
## 
## $`3872`
## <PostgreSQLResult>
## 
## $`3881`
## <PostgreSQLResult>
## 
## $`3885`
## <PostgreSQLResult>
## 
## $`3886`
## <PostgreSQLResult>
## 
## $`3888`
## <PostgreSQLResult>
## 
## $`3891`
## <PostgreSQLResult>
## 
## $`3892`
## <PostgreSQLResult>
## 
## $`3893`
## <PostgreSQLResult>
## 
## $`3894`
## <PostgreSQLResult>
## 
## $`3902`
## <PostgreSQLResult>
## 
## $`3903`
## <PostgreSQLResult>
## 
## $`3906`
## <PostgreSQLResult>
## 
## $`3907`
## <PostgreSQLResult>
## 
## $`3908`
## <PostgreSQLResult>
## 
## $`3909`
## <PostgreSQLResult>
## 
## $`3914`
## <PostgreSQLResult>
## 
## $`3917`
## <PostgreSQLResult>
## 
## $`3921`
## <PostgreSQLResult>
## 
## $`3928`
## <PostgreSQLResult>
## 
## $`3935`
## <PostgreSQLResult>
## 
## $`3938`
## <PostgreSQLResult>
## 
## $`3942`
## <PostgreSQLResult>
## 
## $`3943`
## <PostgreSQLResult>
## 
## $`3946`
## <PostgreSQLResult>
## 
## $`3958`
## <PostgreSQLResult>
## 
## $`3964`
## <PostgreSQLResult>
## 
## $`3972`
## <PostgreSQLResult>
## 
## $`3973`
## <PostgreSQLResult>
## 
## $`3974`
## <PostgreSQLResult>
## 
## $`3981`
## <PostgreSQLResult>
## 
## $`3982`
## <PostgreSQLResult>
## 
## $`3990`
## <PostgreSQLResult>
## 
## $`3992`
## <PostgreSQLResult>
## 
## $`3994`
## <PostgreSQLResult>
## 
## $`4003`
## <PostgreSQLResult>
## 
## $`4005`
## <PostgreSQLResult>
## 
## $`4006`
## <PostgreSQLResult>
## 
## $`4009`
## <PostgreSQLResult>
## 
## $`4031`
## <PostgreSQLResult>
## 
## $`4032`
## <PostgreSQLResult>
## 
## $`4033`
## <PostgreSQLResult>
## 
## $`4035`
## <PostgreSQLResult>
## 
## $`4039`
## <PostgreSQLResult>
## 
## $`4041`
## <PostgreSQLResult>
## 
## $`4047`
## <PostgreSQLResult>
## 
## $`4049`
## <PostgreSQLResult>
## 
## $`4056`
## <PostgreSQLResult>
## 
## $`4058`
## <PostgreSQLResult>
## 
## $`4063`
## <PostgreSQLResult>
## 
## $`4076`
## <PostgreSQLResult>
## 
## $`4078`
## <PostgreSQLResult>
## 
## $`4085`
## <PostgreSQLResult>
## 
## $`4086`
## <PostgreSQLResult>
## 
## $`4089`
## <PostgreSQLResult>
## 
## $`4097`
## <PostgreSQLResult>
## 
## $`4122`
## <PostgreSQLResult>
## 
## $`4123`
## <PostgreSQLResult>
## 
## $`4124`
## <PostgreSQLResult>
## 
## $`4125`
## <PostgreSQLResult>
## 
## $`4142`
## <PostgreSQLResult>
## 
## $`4146`
## <PostgreSQLResult>
## 
## $`4152`
## <PostgreSQLResult>
## 
## $`4153`
## <PostgreSQLResult>
## 
## $`4156`
## <PostgreSQLResult>
## 
## $`4171`
## <PostgreSQLResult>
## 
## $`4178`
## <PostgreSQLResult>
## 
## $`4184`
## <PostgreSQLResult>
## 
## $`4185`
## <PostgreSQLResult>
## 
## $`4191`
## <PostgreSQLResult>
## 
## $`4202`
## <PostgreSQLResult>
## 
## $`4207`
## <PostgreSQLResult>
## 
## $`4211`
## <PostgreSQLResult>
## 
## $`4215`
## <PostgreSQLResult>
## 
## $`4226`
## <PostgreSQLResult>
## 
## $`4234`
## <PostgreSQLResult>
## 
## $`4246`
## <PostgreSQLResult>
## 
## $`4248`
## <PostgreSQLResult>
## 
## $`4250`
## <PostgreSQLResult>
## 
## $`4261`
## <PostgreSQLResult>
## 
## $`4262`
## <PostgreSQLResult>
## 
## $`4265`
## <PostgreSQLResult>
## 
## $`4268`
## <PostgreSQLResult>
## 
## $`4277`
## <PostgreSQLResult>
## 
## $`4288`
## <PostgreSQLResult>
## 
## $`4289`
## <PostgreSQLResult>
## 
## $`4297`
## <PostgreSQLResult>
## 
## $`4299`
## <PostgreSQLResult>
## 
## $`4304`
## <PostgreSQLResult>
## 
## $`4306`
## <PostgreSQLResult>
## 
## $`4307`
## <PostgreSQLResult>
## 
## $`4308`
## <PostgreSQLResult>
## 
## $`4310`
## <PostgreSQLResult>
## 
## $`4320`
## <PostgreSQLResult>
## 
## $`4325`
## <PostgreSQLResult>
## 
## $`4328`
## <PostgreSQLResult>
## 
## $`4332`
## <PostgreSQLResult>
## 
## $`4334`
## <PostgreSQLResult>
## 
## $`4335`
## <PostgreSQLResult>
## 
## $`4341`
## <PostgreSQLResult>
## 
## $`4350`
## <PostgreSQLResult>
## 
## $`4362`
## <PostgreSQLResult>
## 
## $`4364`
## <PostgreSQLResult>
## 
## $`4372`
## <PostgreSQLResult>
## 
## $`4373`
## <PostgreSQLResult>
## 
## $`4376`
## <PostgreSQLResult>
## 
## $`4377`
## <PostgreSQLResult>
## 
## $`4379`
## <PostgreSQLResult>
## 
## $`4380`
## <PostgreSQLResult>
## 
## $`4381`
## <PostgreSQLResult>
## 
## $`4382`
## <PostgreSQLResult>
## 
## $`4386`
## <PostgreSQLResult>
## 
## $`4387`
## <PostgreSQLResult>
## 
## $`4389`
## <PostgreSQLResult>
## 
## $`4392`
## <PostgreSQLResult>
## 
## $`4393`
## <PostgreSQLResult>
## 
## $`4408`
## <PostgreSQLResult>
## 
## $`4414`
## <PostgreSQLResult>
## 
## $`4418`
## <PostgreSQLResult>
## 
## $`4419`
## <PostgreSQLResult>
## 
## $`4420`
## <PostgreSQLResult>
## 
## $`4421`
## <PostgreSQLResult>
## 
## $`4423`
## <PostgreSQLResult>
## 
## $`4434`
## <PostgreSQLResult>
## 
## $`4435`
## <PostgreSQLResult>
## 
## $`4439`
## <PostgreSQLResult>
## 
## $`4440`
## <PostgreSQLResult>
## 
## $`4441`
## <PostgreSQLResult>
## 
## $`4443`
## <PostgreSQLResult>
## 
## $`4445`
## <PostgreSQLResult>
## 
## $`4446`
## <PostgreSQLResult>
## 
## $`4447`
## <PostgreSQLResult>
## 
## $`4456`
## <PostgreSQLResult>
## 
## $`4458`
## <PostgreSQLResult>
## 
## $`4465`
## <PostgreSQLResult>
## 
## $`4485`
## <PostgreSQLResult>
## 
## $`4486`
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
## $`4494`
## <PostgreSQLResult>
## 
## $`4509`
## <PostgreSQLResult>
## 
## $`4518`
## <PostgreSQLResult>
## 
## $`4520`
## <PostgreSQLResult>
## 
## $`4534`
## <PostgreSQLResult>
## 
## $`4541`
## <PostgreSQLResult>
## 
## $`4544`
## <PostgreSQLResult>
## 
## $`4546`
## <PostgreSQLResult>
## 
## $`4558`
## <PostgreSQLResult>
## 
## $`4559`
## <PostgreSQLResult>
## 
## $`4561`
## <PostgreSQLResult>
## 
## $`4566`
## <PostgreSQLResult>
## 
## $`4571`
## <PostgreSQLResult>
## 
## $`4577`
## <PostgreSQLResult>
## 
## $`4586`
## <PostgreSQLResult>
## 
## $`4587`
## <PostgreSQLResult>
## 
## $`4591`
## <PostgreSQLResult>
## 
## $`4608`
## <PostgreSQLResult>
## 
## $`4611`
## <PostgreSQLResult>
## 
## $`4626`
## <PostgreSQLResult>
## 
## $`4629`
## <PostgreSQLResult>
## 
## $`4635`
## <PostgreSQLResult>
## 
## $`4638`
## <PostgreSQLResult>
## 
## $`4639`
## <PostgreSQLResult>
## 
## $`4645`
## <PostgreSQLResult>
## 
## $`4649`
## <PostgreSQLResult>
## 
## $`4652`
## <PostgreSQLResult>
## 
## $`4653`
## <PostgreSQLResult>
## 
## $`4654`
## <PostgreSQLResult>
## 
## $`4656`
## <PostgreSQLResult>
## 
## $`4661`
## <PostgreSQLResult>
## 
## $`4664`
## <PostgreSQLResult>
## 
## $`4677`
## <PostgreSQLResult>
## 
## $`4681`
## <PostgreSQLResult>
## 
## $`4685`
## <PostgreSQLResult>
## 
## $`4689`
## <PostgreSQLResult>
## 
## $`4698`
## <PostgreSQLResult>
## 
## $`4701`
## <PostgreSQLResult>
## 
## $`4712`
## <PostgreSQLResult>
## 
## $`4713`
## <PostgreSQLResult>
## 
## $`4720`
## <PostgreSQLResult>
## 
## $`4736`
## <PostgreSQLResult>
## 
## $`4741`
## <PostgreSQLResult>
## 
## $`4742`
## <PostgreSQLResult>
## 
## $`4745`
## <PostgreSQLResult>
## 
## $`4747`
## <PostgreSQLResult>
## 
## $`4748`
## <PostgreSQLResult>
## 
## $`4754`
## <PostgreSQLResult>
## 
## $`4755`
## <PostgreSQLResult>
## 
## $`4758`
## <PostgreSQLResult>
## 
## $`4759`
## <PostgreSQLResult>
## 
## $`4769`
## <PostgreSQLResult>
## 
## $`4772`
## <PostgreSQLResult>
## 
## $`4777`
## <PostgreSQLResult>
## 
## $`4780`
## <PostgreSQLResult>
## 
## $`4783`
## <PostgreSQLResult>
## 
## $`4792`
## <PostgreSQLResult>
## 
## $`4793`
## <PostgreSQLResult>
## 
## $`4795`
## <PostgreSQLResult>
## 
## $`4797`
## <PostgreSQLResult>
## 
## $`4807`
## <PostgreSQLResult>
## 
## $`4809`
## <PostgreSQLResult>
## 
## $`4825`
## <PostgreSQLResult>
## 
## $`4826`
## <PostgreSQLResult>
## 
## $`4830`
## <PostgreSQLResult>
## 
## $`4834`
## <PostgreSQLResult>
## 
## $`4838`
## <PostgreSQLResult>
## 
## $`4839`
## <PostgreSQLResult>
## 
## $`4840`
## <PostgreSQLResult>
## 
## $`4851`
## <PostgreSQLResult>
## 
## $`4857`
## <PostgreSQLResult>
## 
## $`4858`
## <PostgreSQLResult>
## 
## $`4861`
## <PostgreSQLResult>
## 
## $`4862`
## <PostgreSQLResult>
## 
## $`4865`
## <PostgreSQLResult>
## 
## $`4874`
## <PostgreSQLResult>
## 
## $`4876`
## <PostgreSQLResult>
## 
## $`4879`
## <PostgreSQLResult>
## 
## $`4894`
## <PostgreSQLResult>
## 
## $`4895`
## <PostgreSQLResult>
## 
## $`4897`
## <PostgreSQLResult>
## 
## $`4910`
## <PostgreSQLResult>
## 
## $`4916`
## <PostgreSQLResult>
## 
## $`4917`
## <PostgreSQLResult>
## 
## $`4919`
## <PostgreSQLResult>
## 
## $`4927`
## <PostgreSQLResult>
## 
## $`4931`
## <PostgreSQLResult>
## 
## $`4933`
## <PostgreSQLResult>
## 
## $`4936`
## <PostgreSQLResult>
## 
## $`4938`
## <PostgreSQLResult>
## 
## $`4939`
## <PostgreSQLResult>
## 
## $`4943`
## <PostgreSQLResult>
## 
## $`4944`
## <PostgreSQLResult>
## 
## $`4949`
## <PostgreSQLResult>
## 
## $`4950`
## <PostgreSQLResult>
## 
## $`4951`
## <PostgreSQLResult>
## 
## $`4952`
## <PostgreSQLResult>
## 
## $`4953`
## <PostgreSQLResult>
## 
## $`4958`
## <PostgreSQLResult>
## 
## $`4960`
## <PostgreSQLResult>
## 
## $`4964`
## <PostgreSQLResult>
## 
## $`4968`
## <PostgreSQLResult>
## 
## $`4970`
## <PostgreSQLResult>
## 
## $`4972`
## <PostgreSQLResult>
## 
## $`4980`
## <PostgreSQLResult>
## 
## $`4991`
## <PostgreSQLResult>
## 
## $`4994`
## <PostgreSQLResult>
## 
## $`4996`
## <PostgreSQLResult>
## 
## $`4997`
## <PostgreSQLResult>
## 
## $`4998`
## <PostgreSQLResult>
## 
## $`5010`
## <PostgreSQLResult>
## 
## $`5012`
## <PostgreSQLResult>
## 
## $`5020`
## <PostgreSQLResult>
## 
## $`5025`
## <PostgreSQLResult>
## 
## $`5031`
## <PostgreSQLResult>
## 
## $`5033`
## <PostgreSQLResult>
## 
## $`5034`
## <PostgreSQLResult>
## 
## $`5035`
## <PostgreSQLResult>
## 
## $`5037`
## <PostgreSQLResult>
## 
## $`5044`
## <PostgreSQLResult>
## 
## $`5050`
## <PostgreSQLResult>
## 
## $`5060`
## <PostgreSQLResult>
## 
## $`5073`
## <PostgreSQLResult>
## 
## $`5075`
## <PostgreSQLResult>
## 
## $`5093`
## <PostgreSQLResult>
## 
## $`5098`
## <PostgreSQLResult>
## 
## $`5111`
## <PostgreSQLResult>
## 
## $`5115`
## <PostgreSQLResult>
## 
## $`5118`
## <PostgreSQLResult>
## 
## $`5119`
## <PostgreSQLResult>
## 
## $`5124`
## <PostgreSQLResult>
## 
## $`5131`
## <PostgreSQLResult>
## 
## $`5132`
## <PostgreSQLResult>
## 
## $`5134`
## <PostgreSQLResult>
## 
## $`5142`
## <PostgreSQLResult>
## 
## $`5150`
## <PostgreSQLResult>
## 
## $`5156`
## <PostgreSQLResult>
## 
## $`5157`
## <PostgreSQLResult>
## 
## $`5158`
## <PostgreSQLResult>
## 
## $`5163`
## <PostgreSQLResult>
## 
## $`5170`
## <PostgreSQLResult>
## 
## $`5175`
## <PostgreSQLResult>
## 
## $`5176`
## <PostgreSQLResult>
## 
## $`5178`
## <PostgreSQLResult>
## 
## $`5182`
## <PostgreSQLResult>
## 
## $`5200`
## <PostgreSQLResult>
## 
## $`5202`
## <PostgreSQLResult>
## 
## $`5208`
## <PostgreSQLResult>
## 
## $`5209`
## <PostgreSQLResult>
## 
## $`5217`
## <PostgreSQLResult>
## 
## $`5225`
## <PostgreSQLResult>
## 
## $`5228`
## <PostgreSQLResult>
## 
## $`5229`
## <PostgreSQLResult>
## 
## $`5243`
## <PostgreSQLResult>
## 
## $`5244`
## <PostgreSQLResult>
## 
## $`5249`
## <PostgreSQLResult>
## 
## $`5253`
## <PostgreSQLResult>
## 
## $`5255`
## <PostgreSQLResult>
## 
## $`5259`
## <PostgreSQLResult>
## 
## $`5261`
## <PostgreSQLResult>
## 
## $`5262`
## <PostgreSQLResult>
## 
## $`5263`
## <PostgreSQLResult>
## 
## $`5264`
## <PostgreSQLResult>
## 
## $`5278`
## <PostgreSQLResult>
## 
## $`5279`
## <PostgreSQLResult>
## 
## $`5280`
## <PostgreSQLResult>
## 
## $`5281`
## <PostgreSQLResult>
## 
## $`5284`
## <PostgreSQLResult>
## 
## $`5288`
## <PostgreSQLResult>
## 
## $`5291`
## <PostgreSQLResult>
## 
## $`5294`
## <PostgreSQLResult>
## 
## $`5299`
## <PostgreSQLResult>
## 
## $`5301`
## <PostgreSQLResult>
## 
## $`5302`
## <PostgreSQLResult>
## 
## $`5311`
## <PostgreSQLResult>
## 
## $`5312`
## <PostgreSQLResult>
## 
## $`5313`
## <PostgreSQLResult>
## 
## $`5321`
## <PostgreSQLResult>
## 
## $`5330`
## <PostgreSQLResult>
## 
## $`5339`
## <PostgreSQLResult>
## 
## $`5342`
## <PostgreSQLResult>
## 
## $`5347`
## <PostgreSQLResult>
## 
## $`5349`
## <PostgreSQLResult>
## 
## $`5351`
## <PostgreSQLResult>
## 
## $`5366`
## <PostgreSQLResult>
## 
## $`5378`
## <PostgreSQLResult>
## 
## $`5382`
## <PostgreSQLResult>
## 
## $`5383`
## <PostgreSQLResult>
## 
## $`5386`
## <PostgreSQLResult>
## 
## $`5389`
## <PostgreSQLResult>
## 
## $`5396`
## <PostgreSQLResult>
## 
## $`5399`
## <PostgreSQLResult>
## 
## $`5400`
## <PostgreSQLResult>
## 
## $`5401`
## <PostgreSQLResult>
## 
## $`5408`
## <PostgreSQLResult>
## 
## $`5409`
## <PostgreSQLResult>
## 
## $`5415`
## <PostgreSQLResult>
## 
## $`5425`
## <PostgreSQLResult>
## 
## $`5427`
## <PostgreSQLResult>
## 
## $`5430`
## <PostgreSQLResult>
## 
## $`5433`
## <PostgreSQLResult>
## 
## $`5437`
## <PostgreSQLResult>
## 
## $`5439`
## <PostgreSQLResult>
## 
## $`5443`
## <PostgreSQLResult>
## 
## $`5450`
## <PostgreSQLResult>
## 
## $`5454`
## <PostgreSQLResult>
## 
## $`5457`
## <PostgreSQLResult>
## 
## $`5462`
## <PostgreSQLResult>
## 
## $`5463`
## <PostgreSQLResult>
## 
## $`5465`
## <PostgreSQLResult>
## 
## $`5466`
## <PostgreSQLResult>
## 
## $`5467`
## <PostgreSQLResult>
## 
## $`5475`
## <PostgreSQLResult>
## 
## $`5477`
## <PostgreSQLResult>
## 
## $`5480`
## <PostgreSQLResult>
## 
## $`5482`
## <PostgreSQLResult>
## 
## $`5483`
## <PostgreSQLResult>
## 
## $`5489`
## <PostgreSQLResult>
## 
## $`5496`
## <PostgreSQLResult>
## 
## $`5501`
## <PostgreSQLResult>
## 
## $`5502`
## <PostgreSQLResult>
## 
## $`5504`
## <PostgreSQLResult>
## 
## $`5509`
## <PostgreSQLResult>
## 
## $`5511`
## <PostgreSQLResult>
## 
## $`5513`
## <PostgreSQLResult>
## 
## $`5518`
## <PostgreSQLResult>
## 
## $`5520`
## <PostgreSQLResult>
## 
## $`5524`
## <PostgreSQLResult>
## 
## $`5527`
## <PostgreSQLResult>
## 
## $`5534`
## <PostgreSQLResult>
## 
## $`5544`
## <PostgreSQLResult>
## 
## $`5549`
## <PostgreSQLResult>
## 
## $`5550`
## <PostgreSQLResult>
## 
## $`5555`
## <PostgreSQLResult>
## 
## $`5565`
## <PostgreSQLResult>
## 
## $`5568`
## <PostgreSQLResult>
## 
## $`5570`
## <PostgreSQLResult>
## 
## $`5573`
## <PostgreSQLResult>
## 
## $`5575`
## <PostgreSQLResult>
## 
## $`5578`
## <PostgreSQLResult>
## 
## $`5583`
## <PostgreSQLResult>
## 
## $`5584`
## <PostgreSQLResult>
## 
## $`5586`
## <PostgreSQLResult>
## 
## $`5591`
## <PostgreSQLResult>
## 
## $`5595`
## <PostgreSQLResult>
## 
## $`5601`
## <PostgreSQLResult>
## 
## $`5611`
## <PostgreSQLResult>
## 
## $`5628`
## <PostgreSQLResult>
## 
## $`5633`
## <PostgreSQLResult>
## 
## $`5635`
## <PostgreSQLResult>
## 
## $`5639`
## <PostgreSQLResult>
## 
## $`5641`
## <PostgreSQLResult>
## 
## $`5663`
## <PostgreSQLResult>
## 
## $`5672`
## <PostgreSQLResult>
## 
## $`5674`
## <PostgreSQLResult>
## 
## $`5676`
## <PostgreSQLResult>
## 
## $`5686`
## <PostgreSQLResult>
## 
## $`5701`
## <PostgreSQLResult>
## 
## $`5702`
## <PostgreSQLResult>
## 
## $`5706`
## <PostgreSQLResult>
## 
## $`5708`
## <PostgreSQLResult>
## 
## $`5719`
## <PostgreSQLResult>
## 
## $`5723`
## <PostgreSQLResult>
## 
## $`5725`
## <PostgreSQLResult>
## 
## $`5733`
## <PostgreSQLResult>
## 
## $`5734`
## <PostgreSQLResult>
## 
## $`5742`
## <PostgreSQLResult>
## 
## $`5744`
## <PostgreSQLResult>
## 
## $`5746`
## <PostgreSQLResult>
## 
## $`5747`
## <PostgreSQLResult>
## 
## $`5748`
## <PostgreSQLResult>
## 
## $`5762`
## <PostgreSQLResult>
## 
## $`5765`
## <PostgreSQLResult>
## 
## $`5766`
## <PostgreSQLResult>
## 
## $`5767`
## <PostgreSQLResult>
## 
## $`5770`
## <PostgreSQLResult>
## 
## $`5774`
## <PostgreSQLResult>
## 
## $`5776`
## <PostgreSQLResult>
## 
## $`5778`
## <PostgreSQLResult>
## 
## $`5779`
## <PostgreSQLResult>
## 
## $`5782`
## <PostgreSQLResult>
## 
## $`5787`
## <PostgreSQLResult>
## 
## $`5815`
## <PostgreSQLResult>
## 
## $`5816`
## <PostgreSQLResult>
## 
## $`5817`
## <PostgreSQLResult>
## 
## $`5828`
## <PostgreSQLResult>
## 
## $`5833`
## <PostgreSQLResult>
## 
## $`5834`
## <PostgreSQLResult>
## 
## $`5842`
## <PostgreSQLResult>
## 
## $`5845`
## <PostgreSQLResult>
## 
## $`5851`
## <PostgreSQLResult>
## 
## $`5858`
## <PostgreSQLResult>
## 
## $`5859`
## <PostgreSQLResult>
## 
## $`5863`
## <PostgreSQLResult>
## 
## $`5868`
## <PostgreSQLResult>
## 
## $`5869`
## <PostgreSQLResult>
## 
## $`5874`
## <PostgreSQLResult>
## 
## $`5877`
## <PostgreSQLResult>
## 
## $`5881`
## <PostgreSQLResult>
## 
## $`5882`
## <PostgreSQLResult>
## 
## $`5890`
## <PostgreSQLResult>
## 
## $`5894`
## <PostgreSQLResult>
## 
## $`5896`
## <PostgreSQLResult>
## 
## $`5899`
## <PostgreSQLResult>
## 
## $`5900`
## <PostgreSQLResult>
## 
## $`5904`
## <PostgreSQLResult>
## 
## $`5910`
## <PostgreSQLResult>
## 
## $`5911`
## <PostgreSQLResult>
## 
## $`5918`
## <PostgreSQLResult>
## 
## $`5924`
## <PostgreSQLResult>
## 
## $`5933`
## <PostgreSQLResult>
## 
## $`5936`
## <PostgreSQLResult>
## 
## $`5938`
## <PostgreSQLResult>
## 
## $`5944`
## <PostgreSQLResult>
## 
## $`5945`
## <PostgreSQLResult>
## 
## $`5950`
## <PostgreSQLResult>
## 
## $`5954`
## <PostgreSQLResult>
## 
## $`5958`
## <PostgreSQLResult>
## 
## $`5963`
## <PostgreSQLResult>
## 
## $`5965`
## <PostgreSQLResult>
## 
## $`5966`
## <PostgreSQLResult>
## 
## $`5972`
## <PostgreSQLResult>
## 
## $`5975`
## <PostgreSQLResult>
## 
## $`5977`
## <PostgreSQLResult>
## 
## $`5988`
## <PostgreSQLResult>
## 
## $`5998`
## <PostgreSQLResult>
## 
## $`6000`
## <PostgreSQLResult>
## 
## $`6009`
## <PostgreSQLResult>
## 
## $`6010`
## <PostgreSQLResult>
## 
## $`6015`
## <PostgreSQLResult>
## 
## $`6019`
## <PostgreSQLResult>
## 
## $`6027`
## <PostgreSQLResult>
## 
## $`6028`
## <PostgreSQLResult>
## 
## $`6029`
## <PostgreSQLResult>
## 
## $`6036`
## <PostgreSQLResult>
## 
## $`6039`
## <PostgreSQLResult>
## 
## $`6042`
## <PostgreSQLResult>
## 
## $`6044`
## <PostgreSQLResult>
## 
## $`6047`
## <PostgreSQLResult>
## 
## $`6049`
## <PostgreSQLResult>
## 
## $`6050`
## <PostgreSQLResult>
## 
## $`6054`
## <PostgreSQLResult>
## 
## $`6058`
## <PostgreSQLResult>
## 
## $`6061`
## <PostgreSQLResult>
## 
## $`6064`
## <PostgreSQLResult>
## 
## $`6067`
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
## $`6076`
## <PostgreSQLResult>
## 
## $`6077`
## <PostgreSQLResult>
## 
## $`6079`
## <PostgreSQLResult>
## 
## $`6080`
## <PostgreSQLResult>
## 
## $`6085`
## <PostgreSQLResult>
## 
## $`6090`
## <PostgreSQLResult>
## 
## $`6091`
## <PostgreSQLResult>
## 
## $`6092`
## <PostgreSQLResult>
## 
## $`6093`
## <PostgreSQLResult>
## 
## $`6108`
## <PostgreSQLResult>
## 
## $`6113`
## <PostgreSQLResult>
## 
## $`6115`
## <PostgreSQLResult>
## 
## $`6116`
## <PostgreSQLResult>
## 
## $`6117`
## <PostgreSQLResult>
## 
## $`6120`
## <PostgreSQLResult>
## 
## $`6122`
## <PostgreSQLResult>
## 
## $`6129`
## <PostgreSQLResult>
## 
## $`6140`
## <PostgreSQLResult>
## 
## $`6141`
## <PostgreSQLResult>
## 
## $`6144`
## <PostgreSQLResult>
## 
## $`6150`
## <PostgreSQLResult>
## 
## $`6154`
## <PostgreSQLResult>
## 
## $`6162`
## <PostgreSQLResult>
## 
## $`6168`
## <PostgreSQLResult>
## 
## $`6172`
## <PostgreSQLResult>
## 
## $`6179`
## <PostgreSQLResult>
## 
## $`6186`
## <PostgreSQLResult>
## 
## $`6191`
## <PostgreSQLResult>
## 
## $`6201`
## <PostgreSQLResult>
## 
## $`6206`
## <PostgreSQLResult>
## 
## $`6209`
## <PostgreSQLResult>
## 
## $`6210`
## <PostgreSQLResult>
## 
## $`6226`
## <PostgreSQLResult>
## 
## $`6240`
## <PostgreSQLResult>
## 
## $`6241`
## <PostgreSQLResult>
## 
## $`6249`
## <PostgreSQLResult>
## 
## $`6250`
## <PostgreSQLResult>
## 
## $`6259`
## <PostgreSQLResult>
## 
## $`6268`
## <PostgreSQLResult>
## 
## $`6269`
## <PostgreSQLResult>
## 
## $`6280`
## <PostgreSQLResult>
## 
## $`6281`
## <PostgreSQLResult>
## 
## $`6282`
## <PostgreSQLResult>
## 
## $`6287`
## <PostgreSQLResult>
## 
## $`6294`
## <PostgreSQLResult>
## 
## $`6296`
## <PostgreSQLResult>
## 
## $`6300`
## <PostgreSQLResult>
## 
## $`6301`
## <PostgreSQLResult>
## 
## $`6308`
## <PostgreSQLResult>
## 
## $`6310`
## <PostgreSQLResult>
## 
## $`6312`
## <PostgreSQLResult>
## 
## $`6314`
## <PostgreSQLResult>
## 
## $`6315`
## <PostgreSQLResult>
## 
## $`6316`
## <PostgreSQLResult>
## 
## $`6335`
## <PostgreSQLResult>
## 
## $`6337`
## <PostgreSQLResult>
## 
## $`6341`
## <PostgreSQLResult>
## 
## $`6347`
## <PostgreSQLResult>
## 
## $`6350`
## <PostgreSQLResult>
## 
## $`6351`
## <PostgreSQLResult>
## 
## $`6352`
## <PostgreSQLResult>
## 
## $`6355`
## <PostgreSQLResult>
## 
## $`6357`
## <PostgreSQLResult>
## 
## $`6358`
## <PostgreSQLResult>
## 
## $`6359`
## <PostgreSQLResult>
## 
## $`6367`
## <PostgreSQLResult>
## 
## $`6372`
## <PostgreSQLResult>
## 
## $`6376`
## <PostgreSQLResult>
## 
## $`6387`
## <PostgreSQLResult>
## 
## $`6395`
## <PostgreSQLResult>
## 
## $`6396`
## <PostgreSQLResult>
## 
## $`6401`
## <PostgreSQLResult>
## 
## $`6403`
## <PostgreSQLResult>
## 
## $`6407`
## <PostgreSQLResult>
## 
## $`6410`
## <PostgreSQLResult>
## 
## $`6418`
## <PostgreSQLResult>
## 
## $`6422`
## <PostgreSQLResult>
## 
## $`6425`
## <PostgreSQLResult>
## 
## $`6426`
## <PostgreSQLResult>
## 
## $`6427`
## <PostgreSQLResult>
## 
## $`6429`
## <PostgreSQLResult>
## 
## $`6441`
## <PostgreSQLResult>
## 
## $`6442`
## <PostgreSQLResult>
## 
## $`6445`
## <PostgreSQLResult>
## 
## $`6446`
## <PostgreSQLResult>
## 
## $`6449`
## <PostgreSQLResult>
## 
## $`6451`
## <PostgreSQLResult>
## 
## $`6455`
## <PostgreSQLResult>
## 
## $`6462`
## <PostgreSQLResult>
## 
## $`6466`
## <PostgreSQLResult>
## 
## $`6468`
## <PostgreSQLResult>
## 
## $`6469`
## <PostgreSQLResult>
## 
## $`6475`
## <PostgreSQLResult>
## 
## $`6477`
## <PostgreSQLResult>
## 
## $`6481`
## <PostgreSQLResult>
## 
## $`6482`
## <PostgreSQLResult>
## 
## $`6484`
## <PostgreSQLResult>
## 
## $`6485`
## <PostgreSQLResult>
## 
## $`6490`
## <PostgreSQLResult>
## 
## $`6493`
## <PostgreSQLResult>
## 
## $`6497`
## <PostgreSQLResult>
## 
## $`6500`
## <PostgreSQLResult>
## 
## $`6501`
## <PostgreSQLResult>
## 
## $`6506`
## <PostgreSQLResult>
## 
## $`6507`
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
## $`6518`
## <PostgreSQLResult>
## 
## $`6525`
## <PostgreSQLResult>
## 
## $`6526`
## <PostgreSQLResult>
## 
## $`6528`
## <PostgreSQLResult>
## 
## $`6542`
## <PostgreSQLResult>
## 
## $`6544`
## <PostgreSQLResult>
## 
## $`6549`
## <PostgreSQLResult>
## 
## $`6550`
## <PostgreSQLResult>
## 
## $`6552`
## <PostgreSQLResult>
## 
## $`6560`
## <PostgreSQLResult>
## 
## $`6562`
## <PostgreSQLResult>
## 
## $`6567`
## <PostgreSQLResult>
## 
## $`6577`
## <PostgreSQLResult>
## 
## $`6583`
## <PostgreSQLResult>
## 
## $`6585`
## <PostgreSQLResult>
## 
## $`6595`
## <PostgreSQLResult>
## 
## $`6597`
## <PostgreSQLResult>
## 
## $`6599`
## <PostgreSQLResult>
## 
## $`6621`
## <PostgreSQLResult>
## 
## $`6622`
## <PostgreSQLResult>
## 
## $`6624`
## <PostgreSQLResult>
## 
## $`6625`
## <PostgreSQLResult>
## 
## $`6627`
## <PostgreSQLResult>
## 
## $`6633`
## <PostgreSQLResult>
## 
## $`6645`
## <PostgreSQLResult>
## 
## $`6651`
## <PostgreSQLResult>
## 
## $`6652`
## <PostgreSQLResult>
## 
## $`6659`
## <PostgreSQLResult>
## 
## $`6660`
## <PostgreSQLResult>
## 
## $`6664`
## <PostgreSQLResult>
## 
## $`6667`
## <PostgreSQLResult>
## 
## $`6670`
## <PostgreSQLResult>
## 
## $`6685`
## <PostgreSQLResult>
## 
## $`6692`
## <PostgreSQLResult>
## 
## $`6693`
## <PostgreSQLResult>
## 
## $`6695`
## <PostgreSQLResult>
## 
## $`6701`
## <PostgreSQLResult>
## 
## $`6709`
## <PostgreSQLResult>
## 
## $`6714`
## <PostgreSQLResult>
## 
## $`6719`
## <PostgreSQLResult>
## 
## $`6720`
## <PostgreSQLResult>
## 
## $`6724`
## <PostgreSQLResult>
## 
## $`6725`
## <PostgreSQLResult>
## 
## $`6732`
## <PostgreSQLResult>
## 
## $`6737`
## <PostgreSQLResult>
## 
## $`6742`
## <PostgreSQLResult>
## 
## $`6743`
## <PostgreSQLResult>
## 
## $`6748`
## <PostgreSQLResult>
## 
## $`6750`
## <PostgreSQLResult>
## 
## $`6753`
## <PostgreSQLResult>
## 
## $`6767`
## <PostgreSQLResult>
## 
## $`6775`
## <PostgreSQLResult>
## 
## $`6780`
## <PostgreSQLResult>
## 
## $`6781`
## <PostgreSQLResult>
## 
## $`6783`
## <PostgreSQLResult>
## 
## $`6792`
## <PostgreSQLResult>
## 
## $`6794`
## <PostgreSQLResult>
## 
## $`6799`
## <PostgreSQLResult>
## 
## $`6800`
## <PostgreSQLResult>
## 
## $`6801`
## <PostgreSQLResult>
## 
## $`6802`
## <PostgreSQLResult>
## 
## $`6808`
## <PostgreSQLResult>
## 
## $`6813`
## <PostgreSQLResult>
## 
## $`6819`
## <PostgreSQLResult>
## 
## $`6827`
## <PostgreSQLResult>
## 
## $`6828`
## <PostgreSQLResult>
## 
## $`6835`
## <PostgreSQLResult>
## 
## $`6837`
## <PostgreSQLResult>
## 
## $`6839`
## <PostgreSQLResult>
## 
## $`6842`
## <PostgreSQLResult>
```

```r
na1 <- nrow(user.outlier)
# Outliers identified:
na1
```

```
## [1] 1507
```

```r
# Propotion (%) of outliers:
round(na1/sum(!is.na(user.continent_score$idi)) * 100, 1)
```

```
## [1] 22
```

Total outliers: 1507 out of 6846



