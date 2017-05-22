suppressMessages(library(RODBC))

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
table1 <- "TWITTER.ZZ_USERS"
table2 <- "TWITTER.ZZ_USERS_ENRICH"

#sql1 <- paste("SELECT U.ID, U.SCREENNAME, U.LONGITUDE AS LONG1, U.LATITUDE AS LAT1, L.LONGITUDE AS LONG2, L.LATITUDE AS LAT2, LOWER(TRIM(U.LOCATION)) LOCATION FROM ",table1," U JOIN TWITTER.SMP_LOCATION L ON LOWER(TRIM(L.LOCATION)) = LOWER(TRIM(U.LOCATION)) WHERE U.LATITUDE IS NOT NULL AND L.LATITUDE IS NOT NULL",sep="")
sql1 <- paste("SELECT U.ID, U.SCREENNAME, U.LONGITUDE AS LONG1, U.LATITUDE AS LAT1, L.\"lon\" AS LONG2, L.\"lat\" AS LAT2, UPPER(TRIM(U.LOCATION)) LOCATION FROM ",table1," U JOIN TWITTER.SMP_LOCATION_C L ON UPPER(TRIM(L.\"location\")) = UPPER(TRIM(U.LOCATION)) WHERE U.LATITUDE IS NOT NULL AND L.\"lat\" IS NOT NULL
",sep="")

#' ###Load data
#+ get_data
tl <- system.time(data.latlon <- sqlQuery(myconn, sql1) )


# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
haversine <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  
  #actual formule
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

hf_latlon <- function(x, myconn, t) {
  d <- haversine(as.numeric(x["longitude"]),as.numeric(x["latitude"]),as.numeric(x["lon"]),as.numeric(x["lat"]))
  
  #print(d)
  sql <- paste("update ",t," set DISTANCE_LOCATION=",d,sep="")
  sql <- paste(sql," where ID='",x["user_id"] ,"'",sep="")
  sql <- paste(sql, " and SCREENNAME='",x["screenname"],"'",sep="")
  sqlQuery(myconn, sql)
}


#latlon vs timezone geo (deception)
data.latlon_clean <- na.omit(data.frame(data.latlon$ID, data.latlon$SCREENNAME, data.latlon$LONG1, data.latlon$LAT1, data.latlon$LONG2, data.latlon$LAT2))
colnames(data.latlon_clean) <- c("user_id", "screenname", "longitude", "latitude", "lon", "lat")
apply(data.latlon_clean, 1, hf_latlon, myconn, table2)

close(myconn)
