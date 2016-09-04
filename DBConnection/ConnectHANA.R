#install.packages("RODBC")
library(RODBC) 

####get locale
Sys.getlocale()

#### connect to DB
myconn<-odbcConnect("FSOC", uid="xxx", pwd="xxx", believeNRows=FALSE, rows_at_time=20000, DBMSencoding="UTF-8") 

#list all tables
tb <- sqlTables(myconn)

dataset <- sqlQuery(myconn, "select * from TWITTER.TEST_TWEETS;")
dataset

head(dataset)

####close DB connection
close(myconn) 
