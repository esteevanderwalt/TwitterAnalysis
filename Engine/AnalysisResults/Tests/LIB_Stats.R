chi <- function(data) {
  cat("\n")
  print("Results of chi square test")
  print("=================================")  
  
  d <- data
  options("scipen"=100, "digits"=4)
  attach(d)
  #for each name in dataset except CLASS
  for (i in colnames(d)) {
    print(i)
    if(i != "CLASS"){
      mytable <- table(d[,i],CLASS)
      x <- chisq.test(mytable)  
      #save results
      a <- c(i, x$p.value, x$statistic["X-squared"])
      print(x)
      cat("\n")
    }
  }
  detach(d)
  rm(d)
}

wilcoxon <- function(data) {
  cat("\n")
  print("Results of Wilcoxon Signed Rank test")
  print("=================================")  
  
  d <- data
  options("scipen"=100, "digits"=4)
  #for each name in dataset except CLASS
  for (i in colnames(d)) {
    print(i)
    if(i != "CLASS"){
      fake <- data[data$CLASS=='deceptive',i]
      success <- data[data$CLASS!='deceptive',i]
      x <- wilcox.test(success, fake)       
      #mytable <- table(d[,i],CLASS)
      #x <- wilcox.test(mytable) 
      ##save results
      a <- c(i, x$p.value, x$statistic)
      print(x)
      cat("\n")
    }
  }
  rm(d)
}

wilcoxon.freq <- function(data) {
  cat("\n")
  print("Results of Wilcoxon Signed Rank test")
  print("=================================")  
  
  d <- data
  options("scipen"=100, "digits"=4)
  #for each name in dataset except CLASS
  for (i in colnames(d)) {
    print(i)
    if(i != "CLASS"){
      mytable <- table(d[,i],CLASS)
      x <- wilcox.test(mytable)      
      #mytable <- table(d[,i],CLASS)
      #x <- wilcox.test(mytable) 
      ##save results
      a <- c(i, x$p.value, x$statistic)
      print(x)
      cat("\n")
    }
  }
  rm(d)
}

vtest <- function(data) {
  cat("\n")
  print("Results of Variable test")
  print("=================================")  
  
  d <- data
  options("scipen"=100, "digits"=4)
  #for each name in dataset except CLASS
  for (i in colnames(d)) {
    print(i)
    if(i != "CLASS"){
      fake <- data[data$CLASS=='deceptive',i]
      success <- data[data$CLASS!='deceptive',i]
      x <- var.test(success, fake)  
      #save results
      a <- c(i, x$p.value, x$statistic)
      print(x)
      cat("\n")
    }
  }
  rm(d)
}

#print importance of metdata
imp <- function(data) {
  cat("\n")
  print("Information Gain:")
  
  weights <- information.gain(CLASS~., data)
  print(weights)
  
  cat("\n")
  print("Gain ratio:")
  
  weights <- gain.ratio(CLASS~., data)
  print(weights)
  
  #cat("\n")
  #print("Symmetrical uncertainty:")
  
  #weights <- symmetrical.uncertainty(CLASS~., data)
  #print(weights)
  
  #cat("\n")
  #print("OneR:")
  
  #weights <- oneR(CLASS~., data)
  #print(weights)
  
  cat("\n")
  print("Random forest Accuracy:")
  
  weights2 <- random.forest.importance(CLASS ~ ., data, 1)
  print(weights2)
  
  cat("\n")
  print("Random forest Impurity:")
  
  weights2 <- random.forest.importance(CLASS ~ ., data, 2)
  print(weights2)
  
  #cat("\n")
  #print("CFS:")
  
  #weights3 <- cfs(CLASS~., data)
  #print(weights3)
  
  cat("\n")
  print("Chi squared:")
  
  weights3 <- chi.squared(CLASS~., data)
  print(weights3)
  
  #cat("\n")
  #print("Linear correlation:")
  
  #weights4 <- linear.correlation(CLASS~., data)
  #print(weights4)
}
