#load the data of all users
data <- read.csv(file="C://PhD//ProjectsV2//RStudio//TwitterAnalysis//Engine//AnalysisResults//Results//fe_ml_30runs_results.csv",header=TRUE,sep=",");

c <- ncol(data)
r = c(0,0,0)
#for each column
for(i in 2:c){
  #write down column name
  print(colnames(data)[i])
  #reset win-draw-loss
  r = c(0,0,0)
  #test with each other column
  for(j in 2:c){
    if(j!=i){
      #do wilcoxon test
      #test for win
      w <- wilcox.test(data[,i],data[,j])
      #print(w$p.value)
      if(w$p.value < 0.05){
        #significat
        if(mean(data[,i])>mean(data[,j])){
          r[1] <- r[1] + 1  
        }else{
          r[3] <- r[3] + 1
        }
      }else{
        #test for loss
        r[2] <- r[2] + 1
      }
    }
  }
  #write results of win-draw-loss
  print(paste(r[1],r[2],r[3],sep="-"))
}

mean(data[,8])
w <- wilcox.test(x=data[,8],y=data[,2])
w
w$p.value
w$p.value < 0.05
