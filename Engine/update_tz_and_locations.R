#cities <- c("ARNHEM","ATHENS","BAAR","CBRRE")
library(ggmap)
#geocode(cities)

#load libraries
library(RPostgreSQL)

#loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
#creates a connection to the postgres database
#note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "twitter",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "")

rm(pw) # removes the password
dataset <- dbGetQuery(con, "SELECT * from main.timezone")
dataset
tmp <- dataset

#tmp <- dataset[1:3,]
#tmp

lonlat <- geocode(tmp$timezone, output = "more")
lonlat
dataset2 <- cbind(tmp[,1],lonlat)
colnames(dataset2)[1] <- "timezone"
dataset2

#dbWriteTable(con, c("main","timezone_r"), value=dataset2,overwrite=TRUE,row.names=FALSE)
#dbWriteTable(con, c("tmp","test_tbl_out"), value=myTable,append=TRUE, row.names=FALSE)

#get continents
library(countrycode)
c <- data.frame(countrycode(dataset2$country, "country.name", "continent", warn = FALSE))
colnames(c) <- "continent"
c

dataset2 <- cbind(tmp,lonlat,c)
dataset2

#write the timezone calculated table back to db
dbWriteTable(con, c("main","timezone_r"), value=dataset2,overwrite=TRUE,row.names=FALSE)


##############
## do the same for location

dataset <- dbGetQuery(con, "SELECT * from main.location")
head(dataset)

#tmp <- dataset[1000:1005,]
tmp <- dataset[1:2000,]
nrow(tmp)

lonlat <- geocode(tmp$name, output = "more", messaging=FALSE)
lonlat
#next batch
nrow(dataset) ##to get max rows
tmp2 <- dataset[2001:2738,]
lonlat2 <- geocode(tmp2$name, output = "more")

geocodeQueryCheck(userType = "free")

lonlat
nrow(lonlat) 

#combine two batches
lonlat3 <- lonlat + lonlat2

dataset2 <- cbind(tmp[,1],lonlat3)
colnames(dataset2)[1] <- "location"
dataset2

#get continents
#library(countrycode)
c <- data.frame(countrycode(dataset2$country, "country.name", "continent", warn = FALSE))
colnames(c) <- "continent"
c

dataset2 <- cbind(tmp,lonlat,c)
dataset2

#write the timezone calculated table back to db
dbWriteTable(con, c("main","location_r"), value=dataset2,overwrite=TRUE,row.names=FALSE)

