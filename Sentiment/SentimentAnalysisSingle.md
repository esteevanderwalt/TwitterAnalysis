# Sentiment Analysis with Twitter Data







The aim here is to perform sentiment analysis from the content within tweets. We have various options in processing the content: we can rely on NLP libraries or we can use the language processing capabilities built into the SAP HANA library. We will try both and visually show the results in a wordcloud to measure any noticable differences at first.
We will then create a wordcloud for a select few users and measure there distance from the main word cloud, ie how much does their talk differ to that from the corpus.

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

Connection success: TRUE

##Pull data from the database
We will pull the full corpus but selective fields for analysis due to memory limitations.
Ignore any retweets as we deem this to not be the original content from the user itself and not their opinion.
(we still potentially have to exclude content from bots)


```r
tweets <- dbGetQuery(con, "SELECT \"USERNAME\", \"USERID\", \"CREATEDAT\", \"CONTENT\",\"GEO_ENABLED\", \"LATITUDE\", \"LONGITUDE\", \"LOCATION\", \"TIMEZONE\" from main.experiment_tweets_shortest where \"RETWEET\" = 0 and \"USERNAME\" in ('Londs_','AbeaChou','shift_comma3','HorizonCDT','ttenraBsucraMaD','maknaewon','oliviagarrett25','yallsop','Blackdolphin5','aaleyiahpoisson')")
```

Total amount of tweets in the corpus: 2789

Total unique users: 9

Avg amount of tweets per user: 309.8888889


##Clean the text data
The following code is used to clean the data. We initially just first want to get rid of emoticons.


```r
# extract only content
df <- tweets[tweets$USERNAME == "Londs_", ]
rm(tweets)

df <- data.frame(df$CONTENT)
# rename column
df <- setNames(df, c("CONTENT"))
# showing that there is a lot of junk in the text
head(df)
```

```
##                                                                                                                      CONTENT
## 1                                                                                  @mall0ry_ please tell me you work tonight
## 2                                                                                            I love everything about my life
## 3                                               @mall0ry_ GOD IS ALIVE! Thank ya Jesus. I can't wait to see you Ã­Â Â½Ã­Â¸Â
## 4                                                               This made me smile today Ã¢ÂÂ¤Ã¯Â¸Â http://t.co/DSueGBTPJD
## 5               @JamesAustinCole exactly Ã¢ÂÂºÃ¯Â¸Â chin up. You have so many great things to look forward to Ã­Â Â½Ã­Â¸Â
## 6 @BrytnyAnn aw I agree. It needs to happen more often & I have got to meet that beautiful little girl of yours Ã­Â Â½Ã­Â²Â
```

```r
# get only ASCII characters
df$CONTENT <- sapply(df$CONTENT, function(row) iconv(row, "latin1", "ASCII", 
    sub = ""))
head(df)
```

```
##                                                                                                          CONTENT
## 1                                                                      @mall0ry_ please tell me you work tonight
## 2                                                                                I love everything about my life
## 3                                               @mall0ry_ GOD IS ALIVE! Thank ya Jesus. I can't wait to see you 
## 4                                                               This made me smile today  http://t.co/DSueGBTPJD
## 5                           @JamesAustinCole exactly  chin up. You have so many great things to look forward to 
## 6 @BrytnyAnn aw I agree. It needs to happen more often & I have got to meet that beautiful little girl of yours
```

The we use the text mining libary to remove stopword and also cater for word stemming


```r
# cleanup text
library(tm)
```

```
## Loading required package: NLP
```

```
## 
## Attaching package: 'NLP'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```r
tweetcorpus <- Corpus(DataframeSource(df))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, tolower)
tweetcorpus <- tm_map(tweetcorpus, removeWords, stopwords("english"))
# removeURL <- function(x) gsub('http[^[:space:]]*', '', x) tweetcorpus <-
# tm_map(tweetcorpus, content_transformer(removeURL)) remove word stemming
library(SnowballC)
tweetcorpus <- tm_map(tweetcorpus, stemDocument)
# remove whitespace
tweetcorpus <- tm_map(tweetcorpus, stripWhitespace)
# lastly - treat your preprocessed documents as text documents
tweetcorpus <- tm_map(tweetcorpus, PlainTextDocument)

df <- data.frame(text = unlist(sapply(tweetcorpus, `[`, "content")), stringsAsFactors = F)
df <- setNames(df, c("CONTENT"))

head(df)
```

```
##                                                                 CONTENT
## 1                                      mall0ry please tell work tonight
## 2                                                   love everything lif
## 3                       mall0ry god alive thank ya jesus cant wait see 
## 4                                    made smile today httptcodsuegbtpjd
## 5          jamesaustincole exactly chin many great things look forward 
## 6 brytnyann aw agree needs happen often got meet beautiful little girl
```


##Word clouds
It is then important to count the occurances of all words. We start with one user first to illustrate.


```r
# remove unwanted text from tweets emoticons, hashtags, mentions of people
library(tidytext)
library(stringr)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- df %>% # remove quoted text
filter(!str_detect(CONTENT, "^\"")) %>% # remove hyperlinks mutate(CONTENT = str_replace_all(CONTENT,
# 'https://t.co/[A-Za-z\\d]+|&amp;', '')) %>%
mutate(CONTENT = str_replace_all(CONTENT, "httptco[A-Za-z]", "")) %>% unnest_tokens(word, 
    CONTENT, token = "regex", pattern = reg) %>% filter(!word %in% stop_words$word, 
    str_detect(word, "[a-z]"))

# Amount of words =
nrow(tweet_words)
```

```
## [1] 3331
```

```r
# show results as bar, ordered (top 20)
tweet_words %>% count(word, sort = TRUE) %>% head(20) %>% mutate(word = reorder(word, 
    n)) %>% ggplot(aes(word, n)) + geom_bar(stat = "identity") + ylab("Occurrences") + 
    coord_flip()
```

![](SentimentAnalysisSingle_files/figure-html/worclouds-1.png)<!-- -->

Build a word cloud for the user based on the word counts already done


```r
wf <- tweet_words %>% count(word, sort = TRUE) %>% head(200) %>% mutate(word = reorder(word, 
    n))

# build word cloud
library(wordcloud)
```

```
## Loading required package: RColorBrewer
```

```r
# library(wordcloud2) setting the same seed each time ensures consistent
# look across clouds
set.seed(42)
# limit words by specifying min frequency
pal2 <- brewer.pal(8, "Dark2")
wordcloud(wf$word, wf$n, max.words = 50, random.order = FALSE, colors = pal2)
```

![](SentimentAnalysisSingle_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# wordcloud2(wf[1:50,], size = 0.5,shape = 'circle', color =
# 'random-dark',backgroundColor = 'white')
```

We can also cluster the words together based on Euclidian distance.
This can be presented in various ways:
1. A Dendogram

```r
# Hierarchal Clustering First calculate distance between words & then
# cluster them according to similarity
library(cluster)
dfd <- wf[1:20, ]
labs <- dfd$word  # new labels
rownames(dfd) <- labs
d <- dist(dfd, method = "euclidian")
fit <- hclust(d = d, method = "ward.D")
hcd = as.dendrogram(fit)
```

2. Another version of a dentogram as a tree

```r
plot(hcd, type = "triangle")
# plot.new() plot(fit, hang=-1) groups <- cutree(fit, k=5) # 'k=' defines
# the number of clusters you are using
rect.hclust(fit, k = 5, border = "red")  # draw dendogram with red borders around the 5 clusters   
```

![](SentimentAnalysisSingle_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. A dendogram with a fan shape

```r
# show info as fan diagram
library(ape)
# vector of colors
mypal = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")
# cutting dendrogram in 5 clusters
clus5 = cutree(fit, 5)
# plot
op = par(bg = "white")
# Size reflects miles per gallon
plot(as.phylo(fit), type = "fan", tip.color = mypal[clus5], label.offset = 1, 
    cex = log(dfd$n, 10), col = "red")
```

![](SentimentAnalysisSingle_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

4. Or as a cluster

```r
# show info in cluster
library(fpc)
kfit <- kmeans(d, 3)
clusplot(as.matrix(d), kfit$cluster, color = T, shade = T, labels = 3, lines = 0, 
    main = "Cluster")
```

![](SentimentAnalysisSingle_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


##Sentiment analysis

```r
# sentiment analysis for each of the words
library(tidytext)
library(tidyr)
# get NRC Word-Emotion Association Lexicon
nrc <- sentiments %>% filter(lexicon == "nrc") %>% dplyr::select(word, sentiment)
# show values in dataset
head(nrc)
```

```
## # A tibble: 6 x 2
##        word sentiment
##       <chr>     <chr>
## 1    abacus     trust
## 2   abandon      fear
## 3   abandon  negative
## 4   abandon   sadness
## 5 abandoned     anger
## 6 abandoned      fear
```

```r
# apply sentiment to words
sources <- tweet_words %>% mutate(total_words = n()) %>% ungroup() %>% distinct(word, 
    total_words)

by_source_sentiment <- tweet_words %>% inner_join(nrc, by = "word") %>% count(sentiment, 
    word) %>% ungroup() %>% complete(sentiment, word, fill = list(n = 0)) %>% 
    inner_join(sources) %>% group_by(sentiment, total_words) %>% summarize(words = sum(n)) %>% 
    ungroup()
```

```
## Joining, by = "word"
```

```r
# show the sentiment
by_source_sentiment
```

```
## # A tibble: 10 x 3
##       sentiment total_words words
##           <chr>       <int> <dbl>
## 1         anger        3331   146
## 2  anticipation        3331   226
## 3       disgust        3331   135
## 4          fear        3331   101
## 5           joy        3331   330
## 6      negative        3331   295
## 7      positive        3331   442
## 8       sadness        3331   124
## 9      surprise        3331    85
## 10        trust        3331   187
```

```r
# use Poisson test to measure the difference in sentiment
library(broom)

sentiment_differences <- by_source_sentiment %>% group_by(sentiment) %>% do(tidy(poisson.test(.$words, 
    .$total_words)))

sentiment_differences
```

```
## Source: local data frame [10 x 9]
## Groups: sentiment [10]
## 
##       sentiment   estimate statistic       p.value parameter   conf.low
##           <chr>      <dbl>     <dbl>         <dbl>     <int>      <dbl>
## 1         anger 0.04383068       146 4.940656e-324      3331 0.03700948
## 2  anticipation 0.06784749       226 4.940656e-324      3331 0.05928957
## 3       disgust 0.04052837       135 4.940656e-324      3331 0.03398040
## 4          fear 0.03032122       101 4.940656e-324      3331 0.02469714
## 5           joy 0.09906935       330 4.940656e-324      3331 0.08866764
## 6      negative 0.08856199       295 4.940656e-324      3331 0.07874315
## 7      positive 0.13269289       442 4.940656e-324      3331 0.12060921
## 8       sadness 0.03722606       124 4.940656e-324      3331 0.03096271
## 9      surprise 0.02551786        85 4.940656e-324      3331 0.02038276
## 10        trust 0.05613930       187 4.940656e-324      3331 0.04838104
## # ... with 3 more variables: conf.high <dbl>, method <fctr>,
## #   alternative <fctr>
```

```r
# visualize confidence
library(scales)

sentiment_differences %>% ungroup() %>% mutate(sentiment = reorder(sentiment, 
    estimate)) %>% mutate_each(funs(. - 1), estimate, conf.low, conf.high) %>% 
    ggplot(aes(estimate, sentiment)) + geom_point() + geom_errorbarh(aes(xmin = conf.low, 
    xmax = conf.high)) + scale_x_continuous(labels = percent_format()) + labs(x = "% increase", 
    y = "Sentiment")
```

![](SentimentAnalysisSingle_files/figure-html/sentiment-1.png)<!-- -->

```r
# separate text by sentiment
sents = levels(factor(by_source_sentiment$sentiment))
# get the labels and percents
tweet_words_sentiment <- tweet_words %>% inner_join(nrc, by = "word") %>% count(sentiment, 
    word) %>% ungroup()

labels <- lapply(sents, function(x) paste(x, format(round((length((tweet_words_sentiment[tweet_words_sentiment$sentiment == 
    x, ])$word)/length(tweet_words_sentiment$sentiment) * 100), 2), nsmall = 2), 
    "%"))

# build data for word cloud
nemo = length(sents)
emo.docs = rep("", nemo)
for (i in 1:nemo) {
    tmp = tweet_words_sentiment[by_source_sentiment$sentiment == sents[i], ]$word
    
    emo.docs[i] = paste(tmp, collapse = " ")
}
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
library(wordcloud)
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"), scale = c(3, 0.5), 
    random.order = FALSE, title.size = 1.5)
```

![](SentimentAnalysisSingle_files/figure-html/sentiment-2.png)<!-- -->



