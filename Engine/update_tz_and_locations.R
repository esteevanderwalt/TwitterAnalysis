#cities <- c("ARNHEM","ATHENS","BAAR","CBRRE")
library(ggmap)
library(countrycode)
#geocode(cities)
#load libraries
library(RPostgreSQL)
library(caroline)
library(stringr)

get_geo_for_timezone <- function() {
  
  dataset <- dbGetQuery(con, "SELECT * from main.timezone")
  #dataset
  tmp <- dataset
  a <- nrow(tmp)
  if (a == 0) {
    return(NULL)
  }
  
  tmp <- na.omit(as.data.frame(tmp))
  #tmp <- as.data.frame(tmp[1:5,])
  colnames(tmp)[1] <- "timezone"
  #tmp
  
  lonlat <- geocode(tmp$timezone, output = "more")
  #lonlat
  dataset2 <- cbind(tmp[,1],lonlat)
  colnames(dataset2)[1] <- "timezone"
  #dataset2
  
  #get continents
  c <- data.frame(countrycode(dataset2$country, "country.name", "continent", warn = FALSE))
  colnames(c) <- "continent"
  dataset2 <- cbind(tmp,lonlat,c)
  
  #write the timezone calculated table back to db
  dbWriteTable(con, c("main","timezone_r"), value=dataset2,row.names=FALSE,append=TRUE,overwrite=FALSE)

}

get_location_data <- function(){
  dataset <- dbGetQuery(con, "SELECT location from main.location 
                        where location not in ('rooftop	92-48 dobong-dong') limit 2500") 
  
  #dataset
  tmp <- dataset
  a <- nrow(tmp)
  if (a == 0) {
    return(NULL)
  }
  
  tmp <- na.omit(as.data.frame(tmp))
  colnames(tmp)[1] <- "location"
  return (tmp)
  
}

get_geo_for_location <- function(location) {
  
  ##############
  ## do the same for location
  #first remove all non alphanumerical characters
  location2 <- str_replace_all(location, "[^[:alnum:]]", " ")

  lonlat <- geocode(location2, output = "more", force=TRUE)
  #lonlat
  dataset2 <- as.data.frame(location) #pass matrix to geo function
  dataset2 <- cbind(dataset2,lonlat) # bind with original location
  colnames(dataset2)[1] <- "location"
  #dataset2

  #write nulls otherwise we will always just find them again
  #b <- dataset2$lon
  #if (is.null(b) || is.na(b)){  #did not find coordinates
  #  return (NULL)
  #}
  
  #get continents if country exists
  b <- dataset2$country
  if (!is.null(b)){
    c <- data.frame(countrycode(b, "country.name", "continent", warn = FALSE))
    colnames(c) <- "continent"
    dataset2 <- cbind(dataset2,c)    
  }

  #blank frame
  bframe <- data.frame(location = character(0)
                       ,lon = numeric(0)		
                       ,lat = numeric(0)			
                       ,type = character(0)			
                       ,loctype = character(0)			
                       ,address = character(0)			
                       ,north = numeric(0)			
                       ,south = numeric(0)			
                       ,east = numeric(0)			
                       ,west = numeric(0)			
                       ,locality = character(0)			
                       ,administrative_area_level_2 = character(0)			
                       ,administrative_area_level_1 = character(0)			
                       ,country = character(0)			
                       ,postal_town = character(0)			
                       ,administrative_area_level_4 = character(0)			
                       ,premise = character(0)			
                       ,political = character(0)			
                       ,political.1 = character(0)			
                       ,postal_code = character(0)			
                       ,colloquial_area = character(0)			
                       ,administrative_area_level_3 = character(0)			
                       ,route = character(0)			
                       ,neighborhood = character(0)			
                       ,establishment = character(0)			
                       ,query = character(0)			
                       ,street_number = character(0)			
                       ,continent = character(0))
  
  bframe <- merge(bframe,dataset2,all=TRUE)
  
  #remove column lonlat that gets added somewhere?
  while(ncol(bframe)>28){
    bframe[29] <- NULL
  }
  
  #write the timezone calculated table back to db 
  #dbWriteTable2(con, "location_r", dataset2, fill.null=TRUE, add.id=TRUE, row.names=FALSE,append=TRUE,overwrite=FALSE)
  dbWriteTable(con, c("main","location_r"), value=bframe,row.names=FALSE,append=TRUE,overwrite=FALSE)
  
}

connectdb <- function() {
  #loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  #creates a connection to the postgres database
  #note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "twitter",
                   host = "localhost", port = 5432,
                   user = "postgres", password = "")
  #rm(pw) # removes the password
}

get_geo <- function(x, output) {
  get_geo_for_location(x["location"])
}

flush.console()
#test if connect is open before connecting again
#connectdb()

#get_geo_for_timezone()
data.location <- get_location_data()
#loop through data and do them one for one
apply(data.location, 1, get_geo)
#get_geo_for_location("rooftop	92-48 dobong-dong")

