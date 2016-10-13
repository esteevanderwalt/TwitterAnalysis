# Tweet Account Info







In this section we will explore the user data we have available.
The user data is an aggregation of the information available in the tweets themselves, i.e. only the data pertaining to the account and not the actual tweet.
My aggregating this data we get a view of the account itself.

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

##Get the users

```r
users <- dbGetQuery(con, "SELECT * from main.experiment_user_shortest")
```

Total amount of users in the corpus: 6846


```r
# Preview user dataset
head(users)
```

```
##   userid  username no_of_tweets no_of_replies no_of_retweets no_of_friends
## 1    767      xeni         1000           270            347          3984
## 2  11332    Crissy         1000            19            408           481
## 3  12925    Janina         1000           103             28           899
## 4  18713      al3x         1002           612            153           403
## 5  27953   Busymom         1000           406             45          1912
## 6  30973 Starbucks          497           487              6         99958
##   no_of_followers no_of_devices year_opened geo_enabled
## 1           95486             4        2006           1
## 2           79178             8        2006           0
## 3          137646            13        2006           1
## 4           41133            10        2006           1
## 5           52686             1        2006           0
## 6        11654033             3        2006           1
##               location latitude longitude                   timezone
## 1     where data flows       NA        NA Pacific Time (US & Canada)
## 2                 Ohio       NA        NA Eastern Time (US & Canada)
## 3 all up in your DMs.        NA        NA Pacific Time (US & Canada)
## 4       Portland\\, OR       NA        NA Pacific Time (US & Canada)
## 5            Nashville       NA        NA Central Time (US & Canada)
## 6        Seattle\\, WA       NA        NA Pacific Time (US & Canada)
##                                                                             profile_image
## 1                     http://pbs.twimg.com/profile_images/702536731807973376/iDelKqT6.jpg
## 2                     http://pbs.twimg.com/profile_images/633119610477387776/K9aBQma2.jpg
## 3                     http://pbs.twimg.com/profile_images/583690049067155456/VovjQxPp.jpg
## 4                     http://pbs.twimg.com/profile_images/697959498711830528/glceGyFI.png
## 5 http://pbs.twimg.com/profile_images/1083701007/01598788-cc52-4433-a2b1-01a137c02e11.jpg
## 6                     http://pbs.twimg.com/profile_images/629049594047893504/ALcBvCnr.png
##                                                                  background_image
## 1 http://pbs.twimg.com/profile_background_images/462391105808388097/MMAmbCWF.jpeg
## 2 http://pbs.twimg.com/profile_background_images/378800000180398133/BdnLw2ye.jpeg
## 3 http://pbs.twimg.com/profile_background_images/525130313064669184/rxPZNvVF.jpeg
## 4                                http://abs.twimg.com/images/themes/theme1/bg.png
## 5                               http://abs.twimg.com/images/themes/theme16/bg.gif
## 6 http://pbs.twimg.com/profile_background_images/469156785677291520/33CmaU1r.jpeg
##   is_default_profile_image is_default_background_image
## 1                        0                           0
## 2                        0                           0
## 3                        0                           0
## 4                        0                           1
## 5                        0                           0
## 6                        0                           0
##   is_theme_background_image profile_image_uniqueness
## 1                         0                        1
## 2                         0                        1
## 3                         0                        1
## 4                         1                        1
## 5                         1                        1
## 6                         0                        1
##   background_image_uniqueness
## 1                           1
## 2                           1
## 3                           1
## 4                        3599
## 5                          20
## 6                           1
```

```r
dim(users)
```

```
## [1] 6846   21
```

```r
str(users)
```

```
## 'data.frame':	6846 obs. of  21 variables:
##  $ userid                     : num  767 11332 12925 18713 27953 ...
##  $ username                   : chr  "xeni" "Crissy" "Janina" "al3x" ...
##  $ no_of_tweets               : num  1000 1000 1000 1002 1000 ...
##  $ no_of_replies              : num  270 19 103 612 406 487 504 0 4 34 ...
##  $ no_of_retweets             : num  347 408 28 153 45 6 224 0 250 807 ...
##  $ no_of_friends              : int  3984 481 899 403 1912 99958 177 1203 116 898 ...
##  $ no_of_followers            : int  95486 79178 137646 41133 52686 11654033 292 1038 35448433 332 ...
##  $ no_of_devices              : num  4 8 13 10 1 3 8 1 3 13 ...
##  $ year_opened                : num  2006 2006 2006 2006 2006 ...
##  $ geo_enabled                : int  1 0 1 1 0 1 0 0 0 1 ...
##  $ location                   : chr  "where data flows" "Ohio" "all up in your DMs. " "Portland\\, OR" ...
##  $ latitude                   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ longitude                  : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ timezone                   : chr  "Pacific Time (US & Canada)" "Eastern Time (US & Canada)" "Pacific Time (US & Canada)" "Pacific Time (US & Canada)" ...
##  $ profile_image              : chr  "http://pbs.twimg.com/profile_images/702536731807973376/iDelKqT6.jpg" "http://pbs.twimg.com/profile_images/633119610477387776/K9aBQma2.jpg" "http://pbs.twimg.com/profile_images/583690049067155456/VovjQxPp.jpg" "http://pbs.twimg.com/profile_images/697959498711830528/glceGyFI.png" ...
##  $ background_image           : chr  "http://pbs.twimg.com/profile_background_images/462391105808388097/MMAmbCWF.jpeg" "http://pbs.twimg.com/profile_background_images/378800000180398133/BdnLw2ye.jpeg" "http://pbs.twimg.com/profile_background_images/525130313064669184/rxPZNvVF.jpeg" "http://abs.twimg.com/images/themes/theme1/bg.png" ...
##  $ is_default_profile_image   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ is_default_background_image: int  0 0 0 1 0 0 0 0 0 1 ...
##  $ is_theme_background_image  : int  0 0 0 1 1 0 1 0 0 1 ...
##  $ profile_image_uniqueness   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ background_image_uniqueness: num  1 1 1 3599 20 ...
```

```r
summary(users)
```

```
##      userid            username          no_of_tweets    no_of_replies   
##  Min.   :7.670e+02   Length:6846        Min.   :   1.0   Min.   :   0.0  
##  1st Qu.:2.631e+08   Class :character   1st Qu.: 296.2   1st Qu.:   8.0  
##  Median :9.949e+08   Mode  :character   Median :1000.0   Median :  71.0  
##  Mean   :2.768e+15                      Mean   : 696.0   Mean   : 131.2  
##  3rd Qu.:2.740e+09                      3rd Qu.:1000.0   3rd Qu.: 194.0  
##  Max.   :7.053e+17                      Max.   :3194.0   Max.   :2938.0  
##                                                                          
##  no_of_retweets   no_of_friends     no_of_followers    no_of_devices  
##  Min.   :   0.0   Min.   :      0   Min.   :       0   Min.   : 0.00  
##  1st Qu.:  20.0   1st Qu.:    224   1st Qu.:     198   1st Qu.: 0.00  
##  Median : 153.0   Median :    489   Median :     598   Median : 2.00  
##  Mean   : 233.2   Mean   :   8332   Mean   :  237429   Mean   : 2.57  
##  3rd Qu.: 375.0   3rd Qu.:   1414   3rd Qu.:    5552   3rd Qu.: 4.00  
##  Max.   :1000.0   Max.   :4619567   Max.   :83926159   Max.   :26.00  
##                                                                       
##   year_opened    geo_enabled       location            latitude     
##  Min.   :2006   Min.   :0.0000   Length:6846        Min.   :-38.23  
##  1st Qu.:2011   1st Qu.:0.0000   Class :character   1st Qu.: 34.08  
##  Median :2012   Median :0.0000   Mode  :character   Median : 39.95  
##  Mean   :2012   Mean   :0.4214                      Mean   : 37.91  
##  3rd Qu.:2014   3rd Qu.:1.0000                      3rd Qu.: 43.49  
##  Max.   :2016   Max.   :1.0000                      Max.   : 78.58  
##                                                     NA's   :6074    
##    longitude          timezone         profile_image     
##  Min.   :-124.550   Length:6846        Length:6846       
##  1st Qu.: -87.699   Class :character   Class :character  
##  Median : -79.182   Mode  :character   Mode  :character  
##  Mean   : -53.316                                        
##  3rd Qu.:  -1.028                                        
##  Max.   : 177.412                                        
##  NA's   :6074                                            
##  background_image   is_default_profile_image is_default_background_image
##  Length:6846        Min.   :0.00000          Min.   :0.0000             
##  Class :character   1st Qu.:0.00000          1st Qu.:0.0000             
##  Mode  :character   Median :0.00000          Median :1.0000             
##                     Mean   :0.01855          Mean   :0.5257             
##                     3rd Qu.:0.00000          3rd Qu.:1.0000             
##                     Max.   :1.00000          Max.   :1.0000             
##                                                                         
##  is_theme_background_image profile_image_uniqueness
##  Min.   :0.0000            Min.   : 1.000          
##  1st Qu.:0.0000            1st Qu.: 1.000          
##  Median :1.0000            Median : 1.000          
##  Mean   :0.6404            Mean   : 1.337          
##  3rd Qu.:1.0000            3rd Qu.: 1.000          
##  Max.   :1.0000            Max.   :26.000          
##                                                    
##  background_image_uniqueness
##  Min.   :   0               
##  1st Qu.:   1               
##  Median :3599               
##  Mean   :1902               
##  3rd Qu.:3599               
##  Max.   :3599               
## 
```

```r
# scale the dataset
df <- scale(users[, 3:10])

# Preview scaled dataset
head(df)
```

```
##   no_of_tweets no_of_replies no_of_retweets no_of_friends no_of_followers
## 1    0.7610025     0.8309349      0.4625978   -0.05619278     -0.05946240
## 2    0.7610025    -0.6715718      0.7104633   -0.10146545     -0.06629409
## 3    0.7610025    -0.1687409     -0.8336167   -0.09606323     -0.04180089
## 4    0.7660089     2.8781751     -0.3256957   -0.10247352     -0.08223176
## 5    0.7610025     1.6450421     -0.7645394   -0.08297125     -0.07739202
## 6   -0.4981061     2.1299148     -0.9230108    1.18417252      4.78260119
##   no_of_devices year_opened geo_enabled
## 1     0.4861317   -2.907811   1.1716497
## 2     1.8454937   -2.907811  -0.8533727
## 3     3.5446961   -2.907811   1.1716497
## 4     2.5251746   -2.907811   1.1716497
## 5    -0.5333897   -2.907811  -0.8533727
## 6     0.1462913   -2.907811   1.1716497
```




