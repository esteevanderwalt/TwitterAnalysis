## library to hold common functions used in the IDI engine
suppressMessages(library(reshape))

#CHECK MISSING DATA
# A function that plots missingness

## @knitr part_libs
ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}


########################################
## BUILD SETS
########################################
prepareData <- function(x){
  
  #identify nonzero attributes that can influence result
  nzv <- nearZeroVar(x, saveMetrics= TRUE)
  cat("\n")
  print(nzv[nzv$nzv,])
  
  #check that there are no missing values
  cat("\n")
  print("Confirm missing values")
  print(sapply(x, function(y) sum(is.na(y))))
  
  dmy <- dummyVars("CLASS ~ .", data = x, fullRank=T)
  y <- data.frame(predict(dmy, newdata = x))
  
  #identify correlated predictors
  descrCor <-  cor(y)
  highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .8)
  cat("\n")
  print("Report highly correlated values")
  print(highCorr)
  
  #done automatically?
  #rm(dmy, nzv, descrCor, highCorr)
  y <- cbind(y,x$CLASS)
  colnames(y)[colnames(y) == 'x$CLASS'] <- 'CLASS'
  return(y)
}