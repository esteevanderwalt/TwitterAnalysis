library(RPostgreSQL)

get_users <- function(exp_no, period_no) {
  sql <- paste("SELECT * from main.experiment_user where experiment_no=",exp_no," and period_no=",period_no,sep="")
  dataset <- dbGetQuery(con, sql)
}

name_uniqueness <- function(x, exp_no, period_no) {
  username = x["user_name"]
  sql <- paste("SELECT count(0) from main.experiment_user where user_name='",username,"'",sep="")
  d <- dbGetQuery(con, sql)

  sql <- paste("update main.experiment_user set username_uniqueness=",d,sep="")
  sql <- paste(sql," where user_id='",x["user_id"] ,"'",sep="")
  sql <- paste(sql, " and experiment_no=",exp_no,sep="")
  sql <- paste(sql, " and period_no=",period_no,sep="")
  dbSendQuery(con, sql)
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

apply(data.users, 1, name_uniqueness, exp_no=exp_no, period_no=period_no)

#clear environment variables
#rm(list=ls())
