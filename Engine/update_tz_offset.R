library(RPostgreSQL)
library(geonames)

get_tz <- function() {
  dataset <- dbGetQuery(con, "SELECT * from main.timezone_r where correct_tz is null and timezone <> 'International Date Line West'")
  #timezone like 'Nuku%' and 
}

tz_offset <- function(x) {
  tz <- x["timezone"]
  d <- GNtimezone(as.numeric(x["lat"]),as.numeric(x["lon"]), radius = 0)
    
  #update
  sql <- paste("update main.timezone_r set correct_tz='",d$timezoneId,"'",sep="")
  sql <- paste(sql, " ,gmt_offset=",d["gmtOffset"],sep="")
  sql <- paste(sql, " ,dst_offset=",d["dstOffset"],sep="")
  #sql <- paste(sql," where timezone like 'Nuku%'",sep="")
  sql <- paste(sql," where timezone='",replace(tz,'\'','\'\'') ,"'",sep="")
  dbSendQuery(con, sql)
}

#escape("Nuku'alofa")

options(geonamesUsername="estee.vanderwalt")
#source(system.file("tests","testing.R",package="geonames"),echo=TRUE)
#GNtimezone(30,-90, radius = 0)

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

#get all lat lon data
data.tz <- get_tz()

apply(data.tz, 1, tz_offset)

#clear environment variables
#rm(list=ls())
