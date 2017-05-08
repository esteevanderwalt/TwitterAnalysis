
suppressMessages(library(tm))
suppressMessages(library(dplyr))
suppressMessages(library(tidytext))
suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(SnowballC))  
suppressMessages(library(syuzhet))
suppressMessages(library(reshape))

options(scipen=999)

suppressMessages(library(RODBC))

#get data
fetch <- function(idx, i) {
  
  suppressMessages(library(tm))
  suppressMessages(library(dplyr))
  suppressMessages(library(tidytext))
  suppressMessages(library(tidyr))
  suppressMessages(library(stringr))
  suppressMessages(library(SnowballC))  
  suppressMessages(library(syuzhet))
  suppressMessages(library(reshape))
  
  #print(idx)
  
  #sql <- "SELECT ID, CONTENT FROM TWITTER.EXP_TWEETS_DETAIL"
  #sqltable <- "TWITTER.EXP_TWEETS_DETAIL"
  sqltable <- "TWITTER.ZZ_P_CHATS_WITH_ROWNUM"
  sqlSaveTable <- "TWITTER.EXP_SENTIMENT_CHATS"
  
  start <- (idx * i) - i + 1
  end <- (idx * i)
  sqlstr <- paste("SELECT ID, USERID, CONTENT from ", sqltable, " where rownum >= ", start, " and rownum <= ", end, sep="")
  print(sqlstr)
  
  tw <- sqlQuery(myconn, sqlstr)
  
  
  #create plain dataframe for results
  ff <- as.data.frame(setNames(replicate(16,character(0), simplify = F), letters[1:16]))
  colnames(ff) <- c("ID", "ANGER", "ANTICIPIATION", "DISGUST", "FEAR", "JOY", "SADNESS", "SURPRISE", "TRUST", "POSTIVE","NEGATIVE","BING","AFINN","S","CONTENT","USERID")
  
  #clear any previous results
  ff <- ff[0,]
  
  #print(paste("R_SENTIMENT - TWEET:",tw$ID))
  
  df <- data.frame(tw$ID, tw$USERID, tw$CONTENT)
  #rename column
  df <- setNames(df, c("ID","USERID","CONTENT"))
  #get only ASCII characters
  df$CONTENT <- sapply(df$CONTENT,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  #cleanup text
  tweetcorpus <- Corpus(DataframeSource(df))
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x))
  tweetcorpus <- tm_map(tweetcorpus, removeURL)
  tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
  tweetcorpus <- tm_map(tweetcorpus, tolower) 
  tweetcorpus <- tm_map(tweetcorpus, removeWords, stopwords("english")) 
  #remove word stemming
  tweetcorpus <- tm_map(tweetcorpus, stemDocument)
  #remove whitespace
  tweetcorpus <- tm_map(tweetcorpus, stripWhitespace) 
  #lastly - treat your preprocessed documents as text documents
  tweetcorpus <- tm_map(tweetcorpus, PlainTextDocument) 
  
  #sapply(tweetcorpus, function(x){as.character(x$content[2])})
  #sapply(tweetcorpus, function(x){as.numeric(x$content[1])})
  
  df <-data.frame(id=unlist(sapply(tweetcorpus, function(x){as.numeric(x$content[1])})),
                  userid=unlist(sapply(tweetcorpus, function(x){as.numeric(x$content[2])})),
                  text=unlist(sapply(tweetcorpus, function(x){as.character(x$content[3])})), stringsAsFactors=F)
  #df <-data.frame(text=unlist(sapply(tweetcorpus, `[`, "content")), stringsAsFactors=F)
  df <- setNames(df, c("ID","USERID","CONTENT"))
  
  reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
  tweet_words <- df %>%
    #remove quoted text
    filter(!str_detect(CONTENT, '^"')) %>%
    # #remove hyperlinks
    ##mutate(CONTENT = str_replace_all(CONTENT, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    #mutate(CONTENT = str_replace_all(CONTENT, "httptco[A-Za-z]", "")) %>%
    #mutate(CONTENT = str_replace_all(CONTENT, "httpstco[A-Za-z]", "")) %>%
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
                       count(sentiment, ID, USERID, word) %>%
                       ungroup() %>%
                       #complete(sentiment, ID, USERID, word, fill = list(n = 0)) %>%
                       #inner_join(sources) %>%
                       group_by(ID, USERID, sentiment) %>%
                       summarize(words = sum(n)) %>%
                       ungroup())
    
    t <- cast(data=by_source_sentiment, ID+USERID~sentiment, sum)
  }
  
  final <- t %>%
    inner_join(df, by = c("ID", "USERID")) 
  
  #get other sentiment scores
  ff <-data.frame(ID=final$ID, 
                  ANGER=final$anger,
                  ANTICIPIATION=final$anticipation,
                  DISGUST=final$disgust,
                  FEAR=final$fear,
                  JOY=final$joy,
                  SADNESS=final$sadness,
                  SURPRISE=final$surprise,
                  TRUST=final$trust,
                  POSTIVE=final$positive,
                  NEGATIVE=final$negative,
                  BING=(get_sentiment(as.character(final$CONTENT), method="bing")),
                  AFINN=(get_sentiment(as.character(final$CONTENT), method="afinn")),
                  S=(get_sentiment(as.character(final$CONTENT), method="syuzhet")),
                  CONTENT=final$CONTENT,
                  USERID=final$USERID)
  
  colnames(ff) <- c("ID", "ANGER", "ANTICIPIATION", "DISGUST", "FEAR", "JOY", "SADNESS", "SURPRISE", "TRUST", "POSTIVE","NEGATIVE","BING","AFINN","S","CONTENT","USERID")
  sqlSave(channel=mysconn, ff, tablename=sqlSaveTable, append=TRUE, rownames=FALSE)
  
  #setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")
  ##setwd("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results")
  #write.table(ff, file = paste("ChatSentiment",".csv",sep=""),na="",col.names=FALSE, sep=";", append=TRUE)
  
}

library(foreach)
library(doParallel)

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-2) #not to overload your computer
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(RODBC)
  myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
  mysconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
  NULL
})

#set initial number of chats to investigate
tweets <- 429573
increments <- 1000

iterations <- ceiling(tweets / increments)
#iterations <- 100
l <-c(1:iterations)

parSapply(cl, l, fetch, i=increments)

stopCluster(cl)

#close all potential open connections
odbcCloseAll()