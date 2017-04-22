
suppressMessages(library(tm))
suppressMessages(library(dplyr))
suppressMessages(library(tidytext))
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(SnowballC))  
suppressMessages(library(syuzhet))

coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(length(x)==0)
    x[i] <- y[i]
    x},
    list(...))
}

suppressMessages(library(RODBC))
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
mysconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#sql <- "SELECT ID, CONTENT FROM TWITTER.EXP_TWEETS_DETAIL"
sqltable <- "TWITTER.EXP_TWEETS_DETAIL"
sqlSaveTable <- "TWITTER.EXP_SENTIMENT_TWEETS"

#create plain dataframe for results
ff <- as.data.frame(setNames(replicate(14,character(0), simplify = F), letters[1:14]))
colnames(ff) <- c("ID", "ANGER", "ANTICIPIATION", "DISGUST", "FEAR", "JOY", "SADNESS", "SURPRISE", "TRUST", "POSTIVE","NEGATIVE","BING","AFINN","S")

#get data
tw <- sqlFetch(myconn, sqltable, max = 1, rows_at_time = 1)
#clear any previous results
ff <- ff[0,]

tw <- sqlFetchMore(myconn, max = 1)

while(!is.null(tw)){
  print(paste("R_SENTIMENT - TWEET:",tw$ID))
  
  df <- data.frame(tw$CONTENT)
  #rename column
  df <- setNames(df, c("CONTENT"))
  #get only ASCII characters
  df$CONTENT <- sapply(df$CONTENT,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  #cleanup text
  tweetcorpus <- Corpus(DataframeSource(df))
  tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
  tweetcorpus <- tm_map(tweetcorpus, tolower) 
  tweetcorpus <- tm_map(tweetcorpus, removeWords, stopwords("english")) 
  #removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
  #tweetcorpus <- tm_map(tweetcorpus, content_transformer(removeURL))
  #remove word stemming
  tweetcorpus <- tm_map(tweetcorpus, stemDocument)
  #remove whitespace
  tweetcorpus <- tm_map(tweetcorpus, stripWhitespace) 
  #lastly - treat your preprocessed documents as text documents
  tweetcorpus <- tm_map(tweetcorpus, PlainTextDocument) 
  
  df <-data.frame(text=unlist(sapply(tweetcorpus, `[`, "content")), stringsAsFactors=F)
  df <- setNames(df, c("CONTENT"))
  
  reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
  tweet_words <- df %>%
    #remove quoted text
    filter(!str_detect(CONTENT, '^"')) %>%
    # #remove hyperlinks
    ##mutate(CONTENT = str_replace_all(CONTENT, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    mutate(CONTENT = str_replace_all(CONTENT, "httptco[A-Za-z]", "")) %>%
    mutate(CONTENT = str_replace_all(CONTENT, "httpstco[A-Za-z]", "")) %>%
    unnest_tokens(word, CONTENT, token = "regex", pattern = reg) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "[a-z]"))
  
  #get NRC Word-Emotion Association Lexicon
  nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    dplyr::select(word, sentiment)
  
  sources <- tweet_words %>%
    mutate(total_words = n()) %>%
    ungroup() %>%
    distinct(word, total_words)
  
  test_source_sentiment <- tweet_words %>%
    inner_join(nrc, by = "word") 
  
  test <- nrow(test_source_sentiment)
  
  if(test > 0){
    
    suppressMessages(by_source_sentiment <- tweet_words %>%
                       inner_join(nrc, by = "word") %>%
                       count(sentiment, word) %>%
                       ungroup() %>%
                       complete(sentiment, word, fill = list(n = 0)) %>%
                       inner_join(sources) %>%
                       group_by(sentiment, total_words) %>%
                       summarize(words = sum(n)) %>%
                       ungroup())
    
    #posneg <- by_source_sentiment[by_source_sentiment$sentiment %in% c("positive","negative"),]
    #sentiment <- by_source_sentiment[!by_source_sentiment$sentiment %in% c("positive","negative"),]
    #posneg.sorted <- posneg[rev(order(posneg$words)),]
    #sentiment.sorted <- sentiment[rev(order(sentiment$words)),]
    
    anger <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("anger"),]$words,0)
    anticipation <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("anticipation"),]$words,0)
    disgust <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("disgust"),]$words,0)
    fear <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("fear"),]$words,0)
    joy <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("joy"),]$words,0)
    sadness <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("sadness"),]$words,0)
    surprise <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("surprise"),]$words,0)
    trust <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("trust"),]$words,0)
    positive <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("positive"),]$words,0)
    negative <- coalesce(by_source_sentiment[by_source_sentiment$sentiment %in% c("negative"),]$words,0)
    
  }else{
    #insert 0 values
    anger <- 0
    anticipation <- 0
    disgust <- 0
    fear <- 0
    joy <- 0
    sadness <- 0
    surprise <- 0
    trust <- 0
    positive <- 0
    negative <- 0
  }
  
  #get other sentiment scores
  afinn <- sum(get_sentiment(as.character(tweet_words), method="afinn"))
  bing <- sum(get_sentiment(as.character(tweet_words), method="bing"))
  s <- sum(get_sentiment(as.character(tweet_words), method="syuzhet"))
  
  #insert sentiment scores
  ff <- rbind(ff,list(tw$ID, anger, anticipation, disgust, fear, joy, sadness, surprise, trust, positive, negative, afinn, bing, s))

  colnames(ff) <- c("ID", "ANGER", "ANTICIPIATION", "DISGUST", "FEAR", "JOY", "SADNESS", "SURPRISE", "TRUST", "POSTIVE","NEGATIVE","BING","AFINN","S")
  sqlSave(channel=mysconn, ff, tablename=sqlSaveTable, append=TRUE, rownames=FALSE)
  
  tw <- sqlFetchMore(myconn, max = 1)
  #tw$CONTENT
  #tw <- NULL
  print(!is.null(tw))
}

close(myconn)
close(mysconn)