#cities <- c("ARNHEM","ATHENS","BAAR","CBRRE")
suppressMessages(library(ggmap))
suppressMessages(library(countrycode))
#geocode(cities)
#load libraries
suppressMessages(library(RODBC))
suppressMessages(library(stringr))

get_geo_for_location <- function(location, myconn) {
  
  ##############
  ## do the same for location
  #first remove all non alphanumerical characters
  #location2 <- str_replace_all(location, "[^[:alnum:]]", " ")
  
  lonlat <- geocode(location, output = "more", force=TRUE)
  if(!is.na(lonlat[1])){
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
                           ,political1 = character(0)			
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
      
      #print(bframe)
      #write the timezone calculated table back to db 
      sqlSave(myconn, bframe, tablename="TWITTER.SMP_LOCATION_R",rownames=FALSE,safer=FALSE,append=TRUE,verbose=FALSE,fast=TRUE)
  }  
}

get_geo <- function(x, myconn) {
  #print(x["LOCATION"])
  get_geo_for_location(x["LOCATION"], myconn)
}

options(scipen=999)

#LINUX
myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 

#sql1 <- paste("SELECT LOCATION FROM (	SELECT TRIM(UPPER(TIMEZONE)) AS LOCATION FROM TWITTER.ZZ_USERS	WHERE TIMEZONE IS NOT NULL AND TRIM(TIMEZONE) <> ''	GROUP BY TRIM(UPPER(TIMEZONE))) WHERE LOCATION NOT IN (SELECT \"location\" FROM TWITTER.SMP_LOCATION_R)",sep="")
#sql1 <- paste("SELECT LOCATION FROM (	SELECT TRIM(UPPER(TIMEZONE)) AS LOCATION FROM TWITTER.TWEETS2_USERS_20170517	WHERE TIMEZONE IS NOT NULL AND TRIM(TIMEZONE) <> ''	GROUP BY TRIM(UPPER(TIMEZONE))) WHERE LOCATION NOT IN (SELECT \"location\" FROM TWITTER.SMP_LOCATION_R)",sep="")
sql1 <- paste("SELECT LOCATION FROM (	SELECT TRIM(UPPER(LOCATION)) AS LOCATION FROM TWITTER.ZZ_USERS	WHERE LOCATION IS NOT NULL AND TRIM(LOCATION) <> ''	GROUP BY TRIM(UPPER(LOCATION))) WHERE LOCATION NOT IN (SELECT \"location\" FROM TWITTER.SMP_LOCATION_R)",sep="")
#sql1 <- paste("SELECT LOCATION FROM (	SELECT TRIM(UPPER(LOCATION)) AS LOCATION FROM TWITTER.TWEETS2_USERS_20170517	WHERE LOCATION IS NOT NULL AND TRIM(LOCATION) <> ''	GROUP BY TRIM(UPPER(LOCATION))) WHERE LOCATION NOT IN (SELECT \"location\" FROM TWITTER.SMP_LOCATION_R)",sep="")


#' ###Load data
#+ get_data
tl <- system.time(data.location <- sqlQuery(myconn, sql1) )

#loop through data and do them one for one
apply(data.location, 1, get_geo, myconn)
#get_geo_for_location("rooftop	92-48 dobong-dong")
geocodeQueryCheck(userType="free")
close(myconn)
