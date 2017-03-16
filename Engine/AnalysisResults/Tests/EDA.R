suppressMessages(library(RODBC))
suppressMessages(library(caret))

#only suppose to run on Linux
z <- "LNX"

if(z=="WIN"){
  #WIN
  filename <- "C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/B_WIN_EDA.txt"
}else{  
  #LINUX
  filename <- "~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Results/C_LNX_EDA.txt"
}  

#### connect to DB
if(z=="WIN"){
  #WIN
  myconn<-odbcConnect("FSOC", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
}else{  
  #LINUX
  myconn<-odbcConnect("SAPHANA", uid="SYSTEM", pwd="oEqm66jccx", believeNRows=FALSE, rows_at_time=1, DBMSencoding="UTF-8") 
}  

#' ###Load data
#+ get_data
tl <- system.time(data.original <- sqlQuery(myconn, "SELECT ID, NAME, SCREENNAME, CREATED, ORIGINAL_PROFILE_IMAGE, PROFILE_IMAGE, BACKGROUND_IMAGE, LAST_TWEET, DESCRIPTION, LOCATION, LANGUAGE, FRIENDS_COUNT, FOLLOWERS_COUNT, STATUS_COUNT, LISTED_COUNT, TIMEZONE, UTC_OFFSET, GEO_ENABLED, LATITUDE, LONGITUDE, IS_CELEBRITY, IS_DEFAULT_PROFILE, IS_DEFAULT_PROFILE_IMAGE, IS_BACKGROUND_IMAGE_USED, PROFILE_TEXT_COLOR, PROFILE_BG_COLOR, CLASS from twitter.zz_full_set") )
data.full <- data.original

#load external libraries
if(z=="WIN"){
  #WIN
  setwd("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")
}else{  
  #LINUX
  setwd("~/Projects/RStudio/TwitterAnalysis/Engine/AnalysisResults/Tests")
} 
source("LIB_Cleanup.R")

print("List all attributes available")
print("=============================") 
colnames(data.full)

cat("\n")
print("Amount of data in the dataset")
print("=============================") 
nrow(data.full)

#change factors to characters in dataframe
data.full <- cleanup.factors(data.full)

cat("\n")
print("Data structure")
print("==============") 
str(data.full)
#summary(data.full)

cat("\n")
print("Get unique value count in set")
print("=============================")      
rapply(data.full,function(x)length(unique(x)))

cat("\n")
print("Get null values in set")
print("========================")      
rapply(data.full,function(x)sum(is.na(x)))

cat("\n")
print("Non/Near zero variance")
print("=======================")      
nzv <- nearZeroVar(data.full, saveMetrics= TRUE)
print(nzv[nzv$nzv,])

#Cleanup of twitter data
#which will also convert all data to numeric values
data.clean <- cleanup.Twitter(data.full)
rapply(data.clean,function(x)length(unique(x)))

cat("\n")
print("Correlation")
print("=======================")
#only get numeric fields for correlation
i <- sapply(data.clean, is.numeric)
descrCor <-  cor(data.clean[i])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .75) #more than 75% correlation
print(descrCor)
print(highCorr)

cat("\n")
print("Entropy, IG, IG Ratio")
print("=======================")
#add class column

source("LIB_Entropy.R")
entropy <- function(x,b) {
  #for each column in x
  dfu <- NULL
  for (j in names(x)) {
    print(j)
    if(j != "CLASS"){
      if(is.numeric(x[,j])){
        print(IG_numeric(x, j, "CLASS", bins=b))
        dfu <- rbind(dfu, data.frame(Attribute=j, IG=IG_numeric(x, j, "CLASS", bins=b)))
      }else{
        #print("no")
        dfu <- rbind(dfu, data.frame(Attribute=j, IG=IG_cat(x, j, "CLASS")))
      }
    }
  }
  return(dfu)
}

df <- entropy(data.clean,5)



sink(filename, append = TRUE)

cat("\n")
print("Query loading run time")
print("==============")
print(tl)

sink()
