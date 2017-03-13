library(ggplot2)
library(caret)
library(proxy)

tag <- read.csv("C:/PhD/ProjectsV2/RStudio/TwitterAnalysis/Engine/AnalysisResults/tag_data.csv", row.names = 1)
tag <- as.matrix(tag)

## Select only models for regression
clModels <- tag[tag[,"Classification"] == 1,]

all <- 1:nrow(clModels)
## Seed the analysis with the SVM model
start <- grep("(svmLinear)", rownames(clModels), fixed = TRUE)
pool <- all[all != start]

## Select 4 model models by maximizing the Jaccard
## dissimilarity between sets of models
nextMods <- maxDissim(clModels[start,,drop = FALSE], 
                      clModels[pool, ], 
                      method = "Jaccard",
                      n = 4)

rownames(clModels)[c(start, nextMods)]

if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() 
