#There are different methods to detect the outliers, including standard deviation approach and 
#Tukey’s method which use interquartile (IQR) range approach. In this post I will use the Tukey’s 
#method because I like that it is not dependent on distribution of data. Moreover, the Tukey’s 
#method ignores the mean and standard deviation, which are influenced by the extreme values 
#(outliers).

## @knitr outlierKD, results='hide', message=FALSE, warning=FALSE
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  #response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  #if(response == "y" | response == "yes"){
  #  dt[as.character(substitute(var))] <- invisible(var_name)
  #  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  #  cat("Outliers successfully removed", "n")
  #  return(invisible(dt))
  #} else{
  #  cat("Nothing changed", "n")
  #  return(invisible(var_name))
  #}
  return (dt[dt[var] %in% outlier,])
}