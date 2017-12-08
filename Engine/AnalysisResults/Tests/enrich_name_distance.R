suppressMessages(library(RODBC))
suppressMessages(library(stringdist))
suppressMessages(library(stringr))
suppressMessages(library(stringi))

options(scipen=999)

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
table1 <- "TWITTER.zz_fake_users"
table2 <- "TWITTER.zz_fake_users_enrich"

sql1 <- paste("SELECT U.ID, U.NAME, U.SCREENNAME FROM ",table1, " U JOIN ",table2," E ON E.ID = U.ID and E.SCREENNAME = U.SCREENNAME WHERE E.HAMMING IS NULL",sep="")

#' ###Load data
#+ get_data
tl <- system.time(data.n <- sqlQuery(myconn, sql1) )

#calc hamming and levenshtein distance on names
h <- function(a,b) {
  #if(stri_enc_isutf8(a) && stri_enc_isutf8(b)){
    #pad smaller string with spaces
    if(str_length(a) < str_length(b)){
      a <- str_pad(a, str_length(b), side="right", pad = "0")
    }else{
      b <- str_pad(b, str_length(a), side="right", pad = "0")
    }
    
    h <- stringdist(a,b,method="hamming")
    if(is.infinite(h))
    {
      h <- NULL
    }
  #}else
  #{
  #  h <- NULL
  #}
  return(h)
}

lv <- function(a,b) {
  #if(stri_enc_isutf8(a) && stri_enc_isutf8(b)){
    lv <- stringdist(a,b,method="lv")
  #}else
  #{
  #  lv <- NULL
  #}
  return(lv)
}

sd_calc <- function(x, myconn, t) {
  d1 <- h(x["name"],x["screenname"])
  #print(d1)
  d2 <- lv(x["name"],x["screenname"])
  #print(d2)
  sql <- paste("update ",t," set LEVENSHTEIN=",d1,sep="")
  sql <- paste(sql," , HAMMING=",d2,sep="")
  sql <- paste(sql," where ID='",x["user_id"] ,"'",sep="")
  sql <- paste(sql, " and SCREENNAME='",x["screenname"],"'",sep="")
  sqlQuery(myconn, sql)
}


#latlon vs timezone geo (deception)
data.n_clean <- na.omit(data.frame(data.n$ID, data.n$SCREENNAME, data.n$NAME))
colnames(data.n_clean) <- c("user_id", "screenname", "name")
apply(data.n_clean, 1, sd_calc, myconn, table2)

close(myconn)
