suppressMessages(library(plyr))

#compute Shannon entropy
entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}

#returns IG for numerical variables.
IG_numeric<-function(data, feature, target, bins=4) {
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),]
  #compute entropy for the parent
  e0<-entropy(data[,target])
  
  data$cat<-cut(data[,feature], breaks=bins, labels=c(1:bins))
  
  dd_data<-ddply(data, "cat", here(summarise), 
                 e=entropy(get(target)), 
                 N=length(get(target)),
                 min=min(get(feature)),
                 max=max(get(feature))
  )
  
  #calculate p for each value of feature
  dd_data$p<-dd_data$N/nrow(data)
  #compute IG
  IG<-e0-sum(dd_data$p*dd_data$e)
  
  return(IG)
}

#returns IG for categorical variables.
IG_cat<-function(data,feature,target){
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),] 
  #use ddply to compute e and p for each value of the feature
  dd_data<-ddply(data, feature, here(summarise), e=entropy(get(target)), N=length(get(target)))
  #compute entropy for the parent
  e0<-entropy(data[,target])
  #calculate p for each value of feature
  dd_data$p<-dd_data$N/nrow(data)
  #compute IG
  IG<-e0-sum(dd_data$p*dd_data$e)
  
  return(IG)
}