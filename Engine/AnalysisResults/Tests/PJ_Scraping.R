library(rvest)

#get list of of dates
pj <- read_html("http://www.perverted-justice.com/?archive=byDate&date=list_all")
months <- pj %>% 
  html_nodes("#mainbox br+ a") %>%
  html_attr("href")

months[1]

#for each month
for(i in 1:length(months)) { 
  mm <- paste("http://www.perverted-justice.com",substr(gsub(" ","%20",months[i]), start=2,stop=100),sep="")
  print(mm)
  month <- gsub("./?archive=byDate&date=","",months[i])
  month <- gsub(", ", "",month)
  month <- gsub("./","", month)
  pjl <- read_html(mm)
  #get list of all links on page
  links <- pjl %>% 
    html_nodes("#pedoLink") %>%
    html_attr("href")
  #for each link
  for(j in 1:length(links)) { #length(links)) {
    ll <- paste("http://www.perverted-justice.com",substr(gsub(" ","%20",links[j]), start=2,stop=100),sep="")
    print(ll)
    #ll <- "http://www.perverted-justice.com/?archive=jeff_fisher94"
    #get data for current person
    p <- read_html(ll)
    #get list of all links on page
    #full name
    pp <- p %>% 
      html_nodes(".inText:nth-child(9)") %>%
      html_text()
    fullname <- pp
    #screen name
    pp <- p %>% 
      html_nodes(".inText:nth-child(12)") %>%
      html_text()
    screenname <- pp
    if(length(screenname) == 0L){
      pp <- p %>% 
        html_nodes(".inText:nth-child(11)") %>%
        html_text()
      screenname <- pp
    } 
    if(length(screenname) == 0L){
      screenname = NA
    }    
    #location
    pp <- p %>% 
      html_nodes(".inText:nth-child(14)") %>%
      html_text()
    location <- pp
    if(length(location) == 0L){
      pp <- p %>% 
        html_nodes(".inText:nth-child(15)") %>%
        html_text()
      location <- pp
    }    
    if(length(location) == 0L){
      location = NA
    }
    #age
    pp <- p %>% 
      html_nodes(".inText:nth-child(10)") %>%
      html_text()
    age <- pp
    if(length(age) == 0L){
      age = NA
    }    
    #picture
    pp <- p %>% 
      html_nodes("#pedoPic") %>%
      html_attr("src")
    picture <- pp    
    
    df <- as.data.frame(setNames(replicate(5,character(0), simplify = F), letters[1:5]))
    colnames(df) <- c("fullname", "screenname", "location", "age", "picture")
    df <- rbind(df,list(fullname, screenname, location, age, picture))
    
    #save user info to excel file
    # Write CSV in R
    setwd("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Data")
    write.table(df, file = "PJUserData.csv",na="",col.names=FALSE, sep=";", append=TRUE)
    
    #get chat log
    #chats
    pp <- p %>% 
      html_nodes(".chatLog") 
    chats <- pp    
    
    #first replace unnecessary tags
    s <- gsub("<br>","\r\n",chats)
    #get rid of doubles
    s <- gsub("\r\n\r\n","\r\n",s)
    s <- gsub("\r\n\r\n","\r\n",s)
    #remove everything within tags
    t <- gsub("<(.*?)>","",s)
    #remove any " and ; from text
    t <- gsub(";","|",t)
    t <- gsub("\"","",t)
    #split string
    t <- strsplit(t,"\r\n")
    t <- as.data.frame(t)
    #add columns
    t["username"] = ""
    t["chat"] = ""
    t["date"] = ""
    t["time"] = ""
    colnames(t) <- c("full","username","chat","date","time")
    
    #remove nulls
    deleted <- t$full=="\n"
    t <- t[!deleted,]
    deleted <- t$full==""
    t <- t[!deleted,]
    z <- t
    
    
    #split name from chat
    #z$username <- gsub("([:]+).*", "\\", z$full)
    z$username = substr(z$full,start=1, stop=regexpr(': |- |\\(',z$full) - 1)
    z$date <- as.character(regmatches(z$full,gregexpr("[0-9]+/[0-9]+/[0-9]+",z$full)))
    z$time <- as.character(regmatches(z$full,gregexpr("[0-9]+:[0-9].*[PM|AM]",z$full)))  
    #now get chat
    z$chat <- z$full
    #z$chat <- sub(z$date, "", z$chat)
    #z$chat <- sub("^([0-9]+/[0-9]+/[0-9]+).*", "", z$chat)
    #z$chat <- sub("^([0-9]+:[0-9].*[PM|AM]).*", "", z$chat)
    z$chat <- substr(z$full,start=regexpr(': |- ',z$full) + 1, stop=length(z$full))
    
    #remove whitespace
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    z$full <- trim(z$full)
    z$username <- trim(z$username)
    z$chat <- trim(z$chat)
    z$date <- trim(z$date)
    z$time <- trim(z$time)
    
    #save logs to excel file
    # Write CSV in R
    setwd("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Data")
    write.table(z, file = paste("PJChatData",".csv",sep=""),na="",col.names=FALSE, sep=";", append=TRUE)
    
  }
}





