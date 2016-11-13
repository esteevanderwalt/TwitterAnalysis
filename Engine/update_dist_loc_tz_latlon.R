library(RPostgreSQL)

get_latlon <- function() {
  dataset <- dbGetQuery(con, "SELECT * from main.factor_latlon")  
}

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
haversine <- function(long1, lat1, long2, lat2) {
  #actual formule
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

hf_latlon_tz <- function(x, exp_no, period_no) {
  d <- haversine(as.numeric(x["longitude"]),as.numeric(x["latitude"]),as.numeric(x["lon"]),as.numeric(x["lat"]))

  sql <- paste("update main.experiment_user set dist_latlon_vs_tz=",d,sep="")
  sql <- paste(sql," where user_id='",x["user_id"] ,"'",sep="")
  sql <- paste(sql, " and experiment_no=",exp_no,sep="")
  sql <- paste(sql, " and period_no=",period_no,sep="")
  dbSendQuery(con, sql)
}

hf_latlon_loc <- function(x, exp_no, period_no) {
  d <- haversine(as.numeric(x["longitude"]),as.numeric(x["latitude"]),as.numeric(x["lon"]),as.numeric(x["lat"]))
  
  sql <- paste("update main.experiment_user set dist_latlon_vs_loc=",d,sep="")
  sql <- paste(sql," where user_id='",x["user_id"] ,"'",sep="")
  sql <- paste(sql, " and experiment_no=",exp_no,sep="")
  sql <- paste(sql, " and period_no=",period_no,sep="")
  dbSendQuery(con, sql)
}

hf_tz_loc <- function(x, exp_no, period_no) {
  d <- haversine(as.numeric(x["longitude"]),as.numeric(x["latitude"]),as.numeric(x["lon"]),as.numeric(x["lat"]))
  
  sql <- paste("update main.experiment_user set dist_tz_vs_loc=",d,sep="")
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

#get all lat lon data
data.latlon <- get_latlon()

#perform the hilversum algorithm on each
#score actual vs timezone (show lie)
#id <- 1
#data.latlon_clean$l_lon[id]
#r1 <- hf(data.latlon_clean$longitude[id], data.latlon_clean$latitude[id], data.latlon_clean$l_lon[id], data.latlon_clean$l_lat[id])
exp_no <- 1
period_no <- 1

#latlon vs timezone geo (deception)
data.latlon_clean <- na.omit(data.frame(data.latlon$user_id, data.latlon$longitude, data.latlon$latitude, data.latlon$tz_lon, data.latlon$tz_lat))
colnames(data.latlon_clean) <- c("user_id", "longitude", "latitude", "lon", "lat")
apply(data.latlon_clean, 1, hf_latlon_tz, exp_no=exp_no, period_no=period_no)

#latlon vs location geo (deception)
data.latlon_clean <- na.omit(data.frame(data.latlon$user_id, data.latlon$longitude, data.latlon$latitude, data.latlon$l_lon, data.latlon$l_lat))
colnames(data.latlon_clean) <- c("user_id", "longitude", "latitude", "lon", "lat")
apply(data.latlon_clean, 1, hf_latlon_loc, exp_no=exp_no, period_no=period_no)

#timezone geo vs location geo (potential deception)
data.latlon_clean <- na.omit(data.frame(data.latlon$user_id, data.latlon$tz_lon, data.latlon$tz_lat, data.latlon$l_lon, data.latlon$l_lat))
colnames(data.latlon_clean) <- c("user_id", "longitude", "latitude", "lon", "lat")
apply(data.latlon_clean, 1, hf_tz_loc, exp_no=exp_no, period_no=period_no)


#clear environment variables
#rm(list=ls())
   