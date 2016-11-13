library(RPostgreSQL)
library(tm)
library(SnowballC)   
library(tidytext)
library(tidyr)
library(stringr)
library(dplyr)

get_users <- function(exp_no, period_no) {
  sql <- paste("SELECT * from main.experiment_user where sentiment is null and experiment_no=",exp_no," and period_no=",period_no,sep="")
  dataset <- dbGetQuery(con, sql)
}

user_sentiment <- function(x, exp_no, period_no) {
  userid = x["userid"]
  sql <- paste("SELECT * from main.experiment_tweets_shortest where retweet=0 and userid='",userid,"'",sep="")
  d <- dbGetQuery(con, sql)
  
  if(nrow(d) == 0){
    return(NULL)
  }
  df <- data.frame(d$content)
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
      
    by_source_sentiment <- tweet_words %>%
      inner_join(nrc, by = "word") %>%
      count(sentiment, word) %>%
      ungroup() %>%
      complete(sentiment, word, fill = list(n = 0)) %>%
      inner_join(sources) %>%
      group_by(sentiment, total_words) %>%
      summarize(words = sum(n)) %>%
      ungroup()
    
    posneg <- by_source_sentiment[by_source_sentiment$sentiment %in% c("positive","negative"),]
    sentiment <- by_source_sentiment[!by_source_sentiment$sentiment %in% c("positive","negative"),]
    posneg.sorted <- posneg[rev(order(posneg$words)),]
    sentiment.sorted <- sentiment[rev(order(sentiment$words)),]
    
    #get sentiment score
    ##################  TODO
    #posneg.sorted[1,1]
    #sentiment.sorted[1,1]
    
    sql <- paste("update main.experiment_user set sentiment='",sentiment.sorted[1,1],"'",sep="")
    sql <- paste(sql," , sentiment_pos_neg ='",posneg.sorted[1,1] ,"'",sep="")
    sql <- paste(sql," where userid='",x["userid"] ,"'",sep="")
    sql <- paste(sql, " and experiment_no=",exp_no,sep="")
    sql <- paste(sql, " and period_no=",period_no,sep="")
    dbSendQuery(con, sql)

  }
}

#flush.console()
#test if connect is open before connecting again
if (!exists("con") || isPostgresqlIdCurrent(con)==FALSE){
  #loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  #creates a connection to the postgres database
  #note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "twitter",
                   host = "localhost", port = 5432,
                   user = "postgres", password = "")
  #rm(pw) # removes the password
}

exp_no <- 1
period_no <- 1
#get all users
data.users <- get_users(exp_no, period_no)

apply(data.users, 1, user_sentiment, exp_no=exp_no, period_no=period_no)

#clear environment variables
#rm(list=ls())


