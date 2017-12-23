suppressMessages(library(pROC))

## library to run all 8 algorithms based on given parameters

# function to set up random seeds
setSeeds <- function(method = "cv", numbers = 1, repeats = 1, tunes = NULL, seed = 1237) {
  #B is the number of resamples and integer vector of M (numbers + tune length if any)
  B <- if (method == "cv") numbers
  else if(method == "repeatedcv") numbers * repeats
  else NULL
  
  if(is.null(length)) {
    seeds <- NULL
  } else {
    set.seed(seed = seed)
    seeds <- vector(mode = "list", length = B)
    seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = numbers + ifelse(is.null(tunes), 0, tunes)))
    seeds[[length(seeds) + 1]] <- sample.int(n = 1000000, size = 1)
  }
  # return seeds
  seeds
}

tree_func <- function(final_model, 
                      tree_num, nm) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  pdf(paste("randomforest_",nm,".pdf",sep=""), width = 7, height = 5) #, width = 500, height = 250
  print(plot)
  dev.off()
  
  #print(plot)
}

runDetails <- function(fit, f){
  
  index_class2 <- testing$CLASS == "deceptive"
  index_class1 <- testing$CLASS == "trustworthy"
  
  # Plots
  #plot(fit, main = "Accuracy: fit.m1")
  #plot(varImp(fit), main = "Var Imp: fit.m1")
  
  # In-sample fit
  fit.trn.pred <- predict(fit, newdata = testing)
  assign(paste(f,".trn.pred",sep=""), fit.trn.pred, envir = .GlobalEnv)
  fit.trn.prob <- predict(fit, newdata = testing, type = "prob")
  assign(paste(f,".trn.prob",sep=""), fit.trn.prob, envir = .GlobalEnv)
  fit.trn.cm <- confusionMatrix(fit.trn.pred, testing$CLASS)
  assign(paste(f,".trn.cm",sep=""), fit.trn.cm, envir = .GlobalEnv)
  #fit.trn.cm$table
  #fit.trn.cm$overall[1:2]
  a <- varImp(fit)$importance
  a[,2] <- NULL
  assign(paste(f,".imp",sep=""), a, envir = .GlobalEnv)
  fit.mRoc <- roc(testing$CLASS,fit.trn.prob[,"deceptive"])
  assign(paste(f,".mRoc",sep=""), fit.mRoc, envir = .GlobalEnv)
  if(fit.trn.cm$overall[2]==0){  #cater for cases where there is no curve to draw
    fit.mPR <- pr.curve(fit.trn.prob$deceptive[index_class2], fit.trn.prob$deceptive[index_class1], curve = FALSE)
    fit.mPR$curve <- NULL
  }else{
    fit.mPR <- pr.curve(fit.trn.prob$deceptive[index_class2], fit.trn.prob$deceptive[index_class1], curve = TRUE)  
  }
  
  assign(paste(f,".mPR",sep=""), fit.mPR, envir = .GlobalEnv)
  
  #imagefile <- paste(imagefilename, "ROC_",f,".pdf", sep="")
  #pdf(imagefile, width = 7, height = 6)
  #p <- plot(fit.mRoc)
  #print(p)
  #dev.off()
  #imagefile <- paste(imagefilename, "PR_",f,".pdf", sep="")
  #pdf(imagefile, width = 7, height = 6)
  #p <- plot(fit.mPR)
  #print(p)
  #dev.off()

}

ML_Models_ROC_P <- function(training, resamp, folds, tune, r, samp, filename, imagefilename, ss = 0, summF){

  if(samp=="none"){
    samp <- NULL
  }
  
  #--------------------------------------
  # Model 1 - SVM Radial  -- changed to SVMLinear
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   sigma == constant (0.547204984213807)
  #   C == varied, 3 times (0.25,0.5,1.0)
  
  #svmLinear -> C only as tuning
  
  fit.m1.seeds <- setSeeds(resamp, folds, r, tune)
  
  # Specify fit parameters
  fit.m1.fc <- trainControl(method = resamp,
                            number = folds,
                            repeats = r,
                            seeds = fit.m1.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M1 started")
  sink()
  
  # Build model
  set.seed(123)
  m1.t <- system.time(fit.m1 <- train(CLASS~., data=training,
                                      method = "svmLinear",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m1.fc,
                                      tuneLength = tune))
  runDetails(fit.m1, "fit.m1")
  #fit.m1.trn.cm$overall[1]
  
  sink(filename, append = TRUE)
  print("M1 complete")
  print(m1.t)
  sink()
  
  #--------------------------------------
  # Model 2 - Random forest
  #--------------------------------------
  # Model notes:
  #   warnings() == T
  #   mtry == varied, 3 times (2,4,7)
  
  fit.m2.seeds <- setSeeds(resamp, folds, r, tune)
  
  fit.m2.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m2.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M2 started")
  sink()
  
  # Build model
  set.seed(123)
  m2.t <- system.time(fit.m2 <- train(CLASS~., data=training,
                                      method = "rf",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m2.fc,
                                      tuneLength = tune))
  runDetails(fit.m2, "fit.m2")

  sink(filename, append = TRUE)
  print("M2 complete")
  print(m2.t)
  sink()
  
  #--------------------------------------
  # Model 3 - Decision tree (J48) --changed to C5.0 tree as J48 taking too long
  #--------------------------------------
  # Model notes:
  #   M == varied (1,2,3)
  #   C == varied (0.01, 0.255, 0.5), 3 times
  
  #C5.0 trials, model, winnow
  
  fit.m3.seeds <- setSeeds(resamp, folds, r, tune*tune)
  
  fit.m3.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m3.seeds,
                            classProbs = TRUE,
                            allowParallel = FALSE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M3 started")
  sink()
  
  # Build model
  set.seed(123)
  m3.t <- system.time(fit.m3 <- train(CLASS~., data=training,
                                      method = "J48", #C5.0
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m3.fc,
                                      tuneLength = tune))
  runDetails(fit.m3, "fit.m3")
  
  sink(filename, append = TRUE)
  print("M3 complete")
  print(m3.t)
  sink()
  
  #--------------------------------------
  # Model 4 - Bayesian Network
  #--------------------------------------
  # Model notes:
  #   no tuning
  
  fit.m4.seeds <- setSeeds(resamp, folds, r, 1)
  
  fit.m4.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m4.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M4 started")
  sink()
  
  # Build model
  set.seed(123)
  m4.t <- system.time(fit.m4 <- train(CLASS~., data=training,
                                      method = "bayesglm",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m4.fc))
  runDetails(fit.m4, "fit.m4")
  
  sink(filename, append = TRUE)
  print("M4 complete")
  print(m4.t)
  sink()
  
  #--------------------------------------
  # Model 5 - knn
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   kmax (5,7,9)
  #   distance (fixed = 2)
  #   kernel (fixed = optimal)
  
  #   k == varied, 30 times, each = 1 (knn)
  fit.m5.seeds <- setSeeds(resamp, folds, r, tune)
  
  # Specify fit parameters
  fit.m5.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m5.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M5 started")
  sink()
  
  # Build model
  set.seed(123)
  m5.t <- system.time(fit.m5 <- train(CLASS~., data=training,
                                      method = "kknn",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m5.fc,
                                      tuneLength = tune))
  runDetails(fit.m5, "fit.m5")
  
  sink(filename, append = TRUE)
  print("M5 complete")
  print(m5.t)
  sink()
  
  #--------------------------------------
  # Model 6 - Adaboost -changed to Adabag
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   nlter == varied, 3 times (50, 100, 150)
  #   method == varied, 2 times (Adaboost.M1, Real adaboost)
  
  fit.m6.seeds <- setSeeds(resamp, folds, r, tune*2)
  
  # Specify fit parameters
  fit.m6.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m6.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M6 started")
  sink()
  
  # Build model
  set.seed(123)
  m6.t <- system.time(fit.m6 <- train(CLASS~., data=training,
                                      method = "adaboost", #adaboost
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m6.fc,
                                      tuneLength = tune))
  runDetails(fit.m6, "fit.m6")
  
  sink(filename, append = TRUE)
  print("M6 complete")
  print(m6.t)
  sink()
  
  #--------------------------------------
  # Model 7 - CART
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   cp == varied, 3 times (0.05, 0.13, 0.31)
  
  fit.m7.seeds <- setSeeds(resamp, folds, r, tune)
  
  # Specify fit parameters
  fit.m7.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m7.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M7 started")
  sink()
  
  # Build model
  set.seed(123)
  m7.t <- system.time(fit.m7 <- train(CLASS~., data=training,
                                      method = "rpart",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m7.fc,
                                      tuneLength = tune))
  runDetails(fit.m7, "fit.m7")
  
  sink(filename, append = TRUE)
  print("M7 complete")
  print(m7.t)
  sink()
  
  #--------------------------------------
  # Model 8 - Neural network
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   size 3
  #   decay 3
  
  fit.m8.seeds <- setSeeds(resamp, folds, r, tune*tune)
  
  # Specify fit parameters
  fit.m8.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m8.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)

  sink(filename, append = TRUE)
  print("M8 started")
  sink()
  
  # Build model
  set.seed(123)
  m8.t <- system.time(fit.m8 <- train(CLASS~., data=training,
                                      method = "nnet",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m8.fc,
                                      tuneLength = tune))
  runDetails(fit.m8, "fit.m8")
  
  sink(filename, append = TRUE)
  print("M8 complete")
  print(m8.t)
  sink()

  #save models
  if(ss == 1){
    save(fit.m1,file="pm1")  
    save(fit.m2,file="pm2")  
    
    #library(rJava)
    #.jcache(fit.m3$classifier)
    save(fit.m3,file="pm3")  

    save(fit.m4,file="pm4")  
    save(fit.m5,file="pm5")  
    save(fit.m6,file="pm6")  
    save(fit.m7,file="pm7")  
    save(fit.m8,file="pm8")  
  }

  #--------------------------------------
  # Model Comparison
  #--------------------------------------
  
  # Model Types
  model.types = c("SVM", "tree", "tree", "bayesian", "cluster", "tree", "tree","neuralnet")
  
  # Model Names
  model.names = c("svmLinear", "rf", "J48", "bayesglm", "kknn", "Adaboost", "rpart", "nnet")
  
  model_list <- list(svmRadial = fit.m1,
                     rf = fit.m2,
                     J48 = fit.m3,
                     bayesglm = fit.m4,
                     knn = fit.m5,
                     Adaboost = fit.m6,
                     rpart = fit.m7,
                     nnet = fit.m8
                     )
  
  # Accuracy, Train
  model.trn.acc = rbind(fit.m1.trn.cm$overall[1],
                        fit.m2.trn.cm$overall[1],
                        fit.m3.trn.cm$overall[1],
                        fit.m4.trn.cm$overall[1],
                        fit.m5.trn.cm$overall[1],
                        fit.m6.trn.cm$overall[1],
                        fit.m7.trn.cm$overall[1],
                        fit.m8.trn.cm$overall[1])
  
  # Kappa, Train
  model.trn.kpp = rbind(fit.m1.trn.cm$overall[2],
                        fit.m2.trn.cm$overall[2],
                        fit.m3.trn.cm$overall[2],
                        fit.m4.trn.cm$overall[2],
                        fit.m5.trn.cm$overall[2],
                        fit.m6.trn.cm$overall[2],
                        fit.m7.trn.cm$overall[2],
                        fit.m8.trn.cm$overall[2])
  
  # Sensitivity, Train
  model.trn.sens = rbind(fit.m1.trn.cm$byClass[1],
                         fit.m2.trn.cm$byClass[1],
                         fit.m3.trn.cm$byClass[1],
                         fit.m4.trn.cm$byClass[1],
                         fit.m5.trn.cm$byClass[1],
                         fit.m6.trn.cm$byClass[1],
                         fit.m7.trn.cm$byClass[1],
                         fit.m8.trn.cm$byClass[1])
  
  # Specificity, Train
  model.trn.spec = rbind(fit.m1.trn.cm$byClass[2],
                         fit.m2.trn.cm$byClass[2],
                         fit.m3.trn.cm$byClass[2],
                         fit.m4.trn.cm$byClass[2],
                         fit.m5.trn.cm$byClass[2],
                         fit.m6.trn.cm$byClass[2],
                         fit.m7.trn.cm$byClass[2],
                         fit.m8.trn.cm$byClass[2])
  
  # Precision, Train
  model.trn.prec = rbind(fit.m1.trn.cm$byClass[5],
                         fit.m2.trn.cm$byClass[5],
                         fit.m3.trn.cm$byClass[5],
                         fit.m4.trn.cm$byClass[5],
                         fit.m5.trn.cm$byClass[5],
                         fit.m6.trn.cm$byClass[5],
                         fit.m7.trn.cm$byClass[5],
                         fit.m8.trn.cm$byClass[5])
  
  # Recall, Train
  model.trn.rec = rbind(fit.m1.trn.cm$byClass[6],
                        fit.m2.trn.cm$byClass[6],
                        fit.m3.trn.cm$byClass[6],
                        fit.m4.trn.cm$byClass[6],
                        fit.m5.trn.cm$byClass[6],
                        fit.m6.trn.cm$byClass[6],
                        fit.m7.trn.cm$byClass[6],
                        fit.m8.trn.cm$byClass[6])
  
  # F1, Train
  model.trn.f1 = rbind(fit.m1.trn.cm$byClass[7],
                       fit.m2.trn.cm$byClass[7],
                       fit.m3.trn.cm$byClass[7],
                       fit.m4.trn.cm$byClass[7],
                       fit.m5.trn.cm$byClass[7],
                       fit.m6.trn.cm$byClass[7],
                       fit.m7.trn.cm$byClass[7],
                       fit.m8.trn.cm$byClass[7])
  
  # Prevalence, Train
  model.trn.prev = rbind(fit.m1.trn.cm$byClass[8],
                         fit.m2.trn.cm$byClass[8],
                         fit.m3.trn.cm$byClass[8],
                         fit.m4.trn.cm$byClass[8],
                         fit.m5.trn.cm$byClass[8],
                         fit.m6.trn.cm$byClass[8],
                         fit.m7.trn.cm$byClass[8],
                         fit.m8.trn.cm$byClass[8])
  
  #AUC
  model.trn.auc = rbind(fit.m1.mRoc$auc,
                        fit.m2.mRoc$auc,
                        fit.m3.mRoc$auc,
                        fit.m4.mRoc$auc,
                        fit.m5.mRoc$auc,
                        fit.m6.mRoc$auc,
                        fit.m7.mRoc$auc,
                        fit.m8.mRoc$auc)

  #PR-AUC
  model.trn.prauc = rbind(fit.m1.mPR$auc.integral,
                        fit.m2.mPR$auc.integral,
                        fit.m3.mPR$auc.integral,
                        fit.m4.mPR$auc.integral,
                        fit.m5.mPR$auc.integral,
                        fit.m6.mPR$auc.integral,
                        fit.m7.mPR$auc.integral,
                        fit.m8.mPR$auc.integral)
  
  #Cost
  model.trn.cost = rbind(m1.t[3],
                         m2.t[3],
                         m3.t[3],
                         m4.t[3],
                         m5.t[3],
                         m6.t[3],
                         m7.t[3],
                         m8.t[3])
  
  # Data Frame
  model.comp = data.frame(model.types,
                          model.names,
                          model.trn.acc,
                          model.trn.kpp,
                          model.trn.sens,
                          model.trn.spec,
                          model.trn.prec,
                          model.trn.rec,
                          model.trn.f1,
                          model.trn.prev,
                          model.trn.auc,
                          model.trn.prauc,
                          model.trn.cost)
  rownames(model.comp) = 1:nrow(model.comp)
  colnames(model.comp) = c("Model Type",
                           "Model Name",
                           "Accuracy",
                           "Kappa",
                           "Sensitivity",
                           "Specificity",
                           "Precision",
                           "Recall",
                           "F1",
                           "Prevalence",
                           "AUC",
                           "PR-AUC",
                           "Cost")
  
  model_list <- list(svmRadial = fit.m1.mRoc,
                     rf = fit.m2.mRoc,
                     J48 = fit.m3.mRoc,
                     bayesglm = fit.m4.mRoc,
                     knn = fit.m5.mRoc,
                     Adaboost = fit.m6.mRoc,
                     rpart = fit.m7.mRoc,
                     nnet = fit.m8.mRoc)
  results_list_roc <- list(NA)
  num_mod <- 1
  for(the_roc in model_list){
    results_list_roc[[num_mod]] <- data_frame(tpr = the_roc$sensitivities,
                                              fpr = 1 - the_roc$specificities,
                                              model = names(model_list)[num_mod]) 
    num_mod <- num_mod + 1
  }
  results_df_roc <- bind_rows(results_list_roc)
  imagefile <- paste(imagefilename, "fullROC.pdf", sep="")
  pdf(imagefile, width = 7, height = 6)
  p <- ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) + 
    geom_line(aes(color = model, linetype = model),size = 1) +  
    #scale_color_manual(values = custom_col) +
    geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
    theme_bw(base_size = 18) +
    labs(x="False Positive Rate",y="True Positive Rate", title="ROC Curve")
  print(p)
  dev.off()
  
  model_list_pr <- list(svmRadial = fit.m1.mPR,
                     rf = fit.m2.mPR,
                     J48 = fit.m3.mPR,
                     bayesglm = fit.m4.mPR,
                     knn = fit.m5.mPR,
                     Adaboost = fit.m6.mPR,
                     rpart = fit.m7.mPR,
                     nnet = fit.m8.mPR)
  results_list_pr <- list(NA)
  num_mod <- 1
  for(the_pr in model_list_pr){
    if(!is.null(the_pr$curve)){
      results_list_pr[[num_mod]] <- data.frame(the_pr$curve, model = names(model_list)[num_mod])  
      num_mod <- num_mod + 1
    }
  }
  results_df_pr <- bind_rows(results_list_pr)
  imagefile <- paste(imagefilename, "fullPR.pdf", sep="")
  pdf(imagefile, width = 7, height = 6)
  p <- ggplot(aes(x=X1,y=X2, group = model), data = results_df_pr) + 
    geom_line(aes(color = model, linetype = model),size = 1) +
    geom_abline(intercept = 0.5, slope = 0, color = "gray", size = 1) +
    theme_bw(base_size = 18) +
    labs(x="Recall",y="Precision", title="PR Curve")
  print(p)
  dev.off()
  
  sink(filename, append = TRUE)
  
  cat("\n")
  print("Model engine summary")
  print("====================")
  
  print(model.comp)
  
  cat("\n")
  print("Model attribute importance")
  print("==========================")
  #Show the importance of all variables per model
  merge.all <- function(x, ..., by = "row.names") {
    L <- list(...)
    for (i in seq_along(L)) {
      x <- merge(x, L[[i]], by = by)
      rownames(x) <- x$Row.names
      x$Row.names <- NULL
    }
    return(x)
  }
  
  model.imp <- merge.all(fit.m1.imp,
                         fit.m2.imp,
                         fit.m3.imp,
                         fit.m4.imp,
                         fit.m5.imp,
                         fit.m6.imp,
                         fit.m7.imp,
                         fit.m8.imp)
  
  colnames(model.imp) = c("svmRadial", "rf", "J48", "bayesglm", "knn", "Adaboost", "rpart", "nnet")
  print(model.imp)
  
  cat("\n")
  print("Model engine results")
  print("====================")
  
  print_engine.all <- function(...) {
    L <- list(...)
    for (i in seq_along(L)) {
      cat("\n")
      print("+++++++++++++")
      print(L[[i]]$method)
      print("+++++++++++++")
      print("**** finalModel ****")
      print(L[[i]]$finalModel)
      cat("\n")
      print("**** results ****")
      print(L[[i]]$results)
      cat("\n")
      #confusionmatrix over all runs
      print("**** resampledCM ****")
      print(L[[i]]$resampledCM)
      cat("\n")
      #ROC, Sens, Spec over best tune (always 10x3 results - crossvalidation)
      print("**** resample ****")
      print(L[[i]]$resample)
      cat("\n")
      
    }
  }
  
  print_engine.all(fit.m1,fit.m2,fit.m3,fit.m4,fit.m5,fit.m6,fit.m7,fit.m8)
  
  sink()
  
  
}

ML_Models_ROC_P_speed <- function(training, resamp, folds, tune, r, samp, filename, imagefilename, ss = 0, summF){
  
  if(samp=="none"){
    samp <- NULL
  }
  
  #--------------------------------------
  # Model 2 - Random forest
  #--------------------------------------
  # Model notes:
  #   warnings() == T
  #   mtry == varied, 3 times (2,4,7)
  
  fit.m2.seeds <- setSeeds(resamp, folds, r, tune)
  
  fit.m2.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m2.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)
  
  sink(filename, append = TRUE)
  print("M2 started")
  sink()
  
  # Build model
  set.seed(123)
  m2.t <- system.time(fit.m2 <- train(CLASS~., data=training,
                                      method = "rf",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m2.fc,
                                      tuneLength = tune))
  runDetails(fit.m2, "fit.m2")
  
  sink(filename, append = TRUE)
  print("M2 complete")
  print(m2.t)
  sink()
  
  #--------------------------------------
  # Model 4 - Bayesian Network
  #--------------------------------------
  # Model notes:
  #   no tuning
  
  fit.m4.seeds <- setSeeds(resamp, folds, r, 1)
  
  fit.m4.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m4.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)
  
  sink(filename, append = TRUE)
  print("M4 started")
  sink()
  
  # Build model
  set.seed(123)
  m4.t <- system.time(fit.m4 <- train(CLASS~., data=training,
                                      method = "bayesglm",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m4.fc))
  runDetails(fit.m4, "fit.m4")
  
  sink(filename, append = TRUE)
  print("M4 complete")
  print(m4.t)
  sink()
  
  #--------------------------------------
  # Model 7 - CART
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   cp == varied, 3 times (0.05, 0.13, 0.31)
  
  fit.m7.seeds <- setSeeds(resamp, folds, r, tune)
  
  # Specify fit parameters
  fit.m7.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m7.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)
  
  sink(filename, append = TRUE)
  print("M7 started")
  sink()
  
  # Build model
  set.seed(123)
  m7.t <- system.time(fit.m7 <- train(CLASS~., data=training,
                                      method = "rpart",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m7.fc,
                                      tuneLength = tune))
  runDetails(fit.m7, "fit.m7")
  
  sink(filename, append = TRUE)
  print("M7 complete")
  print(m7.t)
  sink()

  #--------------------------------------
  # Model 8 - Neural network
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   size 3
  #   decay 3
  
  fit.m8.seeds <- setSeeds(resamp, folds, r, tune*tune)
  
  # Specify fit parameters
  fit.m8.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m8.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)
  
  sink(filename, append = TRUE)
  print("M8 started")
  sink()
  
  # Build model
  set.seed(123)
  m8.t <- system.time(fit.m8 <- train(CLASS~., data=training,
                                      method = "nnet",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m8.fc,
                                      tuneLength = tune))
  runDetails(fit.m8, "fit.m8")
  
  sink(filename, append = TRUE)
  print("M8 complete")
  print(m8.t)
  sink()
  
  #--------------------------------------
  # Model Comparison
  #--------------------------------------
  
  # Model Types
  model.types = c("tree", "bayesian", "tree", "neuralnet")
  
  # Model Names
  model.names = c("rf", "bayesglm", "rpart", "nnet")
  
  model_list <- list(rf = fit.m2,
                     bayesglm = fit.m4,
                     rpart = fit.m7,
                     nnet = fit.m8
  )
  
  # Accuracy, Train
  model.trn.acc = rbind(fit.m2.trn.cm$overall[1],
                        fit.m4.trn.cm$overall[1],
                        fit.m7.trn.cm$overall[1],
                        fit.m8.trn.cm$overall[1])
  
  # Kappa, Train
  model.trn.kpp = rbind(fit.m2.trn.cm$overall[2],
                        fit.m4.trn.cm$overall[2],
                        fit.m7.trn.cm$overall[2],
                        fit.m8.trn.cm$overall[2])
  
  # Sensitivity, Train
  model.trn.sens = rbind(fit.m2.trn.cm$byClass[1],
                         fit.m4.trn.cm$byClass[1],
                         fit.m7.trn.cm$byClass[1],
                         fit.m8.trn.cm$byClass[1])
  
  # Specificity, Train
  model.trn.spec = rbind(fit.m2.trn.cm$byClass[2],
                         fit.m4.trn.cm$byClass[2],
                         fit.m7.trn.cm$byClass[2],
                         fit.m8.trn.cm$byClass[2])
  
  # Precision, Train
  model.trn.prec = rbind(fit.m2.trn.cm$byClass[5],
                         fit.m4.trn.cm$byClass[5],
                         fit.m7.trn.cm$byClass[5],
                         fit.m8.trn.cm$byClass[5])
  
  # Recall, Train
  model.trn.rec = rbind(fit.m2.trn.cm$byClass[6],
                        fit.m4.trn.cm$byClass[6],
                        fit.m7.trn.cm$byClass[6],
                        fit.m8.trn.cm$byClass[6])
  
  # F1, Train
  model.trn.f1 = rbind(fit.m2.trn.cm$byClass[7],
                       fit.m4.trn.cm$byClass[7],
                       fit.m7.trn.cm$byClass[7],
                       fit.m8.trn.cm$byClass[7])
  
  # Prevalence, Train
  model.trn.prev = rbind(fit.m2.trn.cm$byClass[8],
                         fit.m4.trn.cm$byClass[8],
                         fit.m7.trn.cm$byClass[8],
                         fit.m8.trn.cm$byClass[8])
  
  #AUC
  model.trn.auc = rbind(fit.m2.mRoc$auc,
                        fit.m4.mRoc$auc,
                        fit.m7.mRoc$auc,
                        fit.m8.mRoc$auc)
  
  #PR-AUC
  model.trn.prauc = rbind(fit.m2.mPR$auc.integral,
                          fit.m4.mPR$auc.integral,
                          fit.m7.mPR$auc.integral,
                          fit.m8.mPR$auc.integral)
  
  #Cost
  model.trn.cost = rbind(m2.t[3],
                         m4.t[3],
                         m7.t[3],
                         m8.t[3])
  
  # Data Frame
  model.comp = data.frame(model.types,
                          model.names,
                          model.trn.acc,
                          model.trn.kpp,
                          model.trn.sens,
                          model.trn.spec,
                          model.trn.prec,
                          model.trn.rec,
                          model.trn.f1,
                          model.trn.prev,
                          model.trn.auc,
                          model.trn.prauc,
                          model.trn.cost)
  rownames(model.comp) = 1:nrow(model.comp)
  colnames(model.comp) = c("Model Type",
                           "Model Name",
                           "Accuracy",
                           "Kappa",
                           "Sensitivity",
                           "Specificity",
                           "Precision",
                           "Recall",
                           "F1",
                           "Prevalence",
                           "AUC",
                           "PR-AUC",
                           "Cost")
  
  model_list <- list(rf = fit.m2.mRoc,
                     bayesglm = fit.m4.mRoc,
                     rpart = fit.m7.mRoc,
                     nnet = fit.m8.mRoc)
  
  sink(filename, append = TRUE)
  
  cat("\n")
  print("Model engine summary")
  print("====================")
  
  print(model.comp)
  
  cat("\n")
  print("Model attribute importance")
  print("==========================")
  #Show the importance of all variables per model
  merge.all <- function(x, ..., by = "row.names") {
    L <- list(...)
    for (i in seq_along(L)) {
      x <- merge(x, L[[i]], by = by)
      rownames(x) <- x$Row.names
      x$Row.names <- NULL
    }
    return(x)
  }
  
  model.imp <- merge.all(fit.m2.imp,
                         fit.m4.imp,
                         fit.m7.imp,
                         fit.m8.imp)
  
  colnames(model.imp) = c("rf", "bayesglm", "rpart", "nnet")
  print(model.imp)
  
  cat("\n")
  print("Model engine results")
  print("====================")
  
  print_engine.all <- function(...) {
    L <- list(...)
    for (i in seq_along(L)) {
      cat("\n")
      print("+++++++++++++")
      print(L[[i]]$method)
      print("+++++++++++++")
      print("**** finalModel ****")
      print(L[[i]]$finalModel)
      cat("\n")
      print("**** results ****")
      print(L[[i]]$results)
      cat("\n")
      #confusionmatrix over all runs
      print("**** resampledCM ****")
      print(L[[i]]$resampledCM)
      cat("\n")
      #ROC, Sens, Spec over best tune (always 10x3 results - crossvalidation)
      print("**** resample ****")
      print(L[[i]]$resample)
      cat("\n")
      
    }
  }
  
  print_engine.all(fit.m2,fit.m4,fit.m7,fit.m8)
  
  sink()
  
  
}

ML_Models_ROC_P_one <- function(training, resamp, folds, tune, r, samp, filename, imagefilename, ss = 0, summF){
  
  if(samp=="none"){
    samp <- NULL
  }
  
  #--------------------------------------
  # just run one model
  #--------------------------------------
  # Model notes:
  #   warnings() == T
  #   mtry == varied, 3 times (2,4,7)
  
  fit.m2.seeds <- setSeeds(resamp, folds, r, tune)
  
  fit.m2.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m2.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)
  
  sink(filename, append = TRUE)
  print("M2 started")
  sink()
  
  # Build model
  set.seed(123)
  m2.t <- system.time(fit.m2 <- train(CLASS~., data=training,
                                      method = "rf",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m2.fc,
                                      tuneLength = tune))
  runDetails(fit.m2, "fit.m2")
  
  sink(filename, append = TRUE)
  print("M2 complete")
  print(m2.t)
  sink()
  
  #--------------------------------------
  # Model Comparison
  #--------------------------------------
  
  # Model Types
  model.types = c("type")
  
  # Model Names
  model.names = c("model")
  
  # Accuracy, Train
  model.trn.acc = rbind(fit.m2.trn.cm$overall[1])
  
  # Kappa, Train
  model.trn.kpp = rbind(fit.m2.trn.cm$overall[2])
  
  # Sensitivity, Train
  model.trn.sens = rbind(fit.m2.trn.cm$byClass[1])
  
  # Specificity, Train
  model.trn.spec = rbind(fit.m2.trn.cm$byClass[2])
  
  # Precision, Train
  model.trn.prec = rbind(fit.m2.trn.cm$byClass[5])
  
  # Recall, Train
  model.trn.rec = rbind(fit.m2.trn.cm$byClass[6])
  
  # F1, Train
  model.trn.f1 = rbind(fit.m2.trn.cm$byClass[7])
  
  # Prevalence, Train
  model.trn.prev = rbind(fit.m2.trn.cm$byClass[8])
  
  #AUC
  model.trn.auc = rbind(fit.m2.mRoc$auc)
  
  #Cost
  model.trn.cost = rbind(m2.t[3])
  
  # Data Frame
  model.comp = data.frame(model.types,
                          model.names,
                          model.trn.acc,
                          model.trn.kpp,
                          model.trn.sens,
                          model.trn.spec,
                          model.trn.prec,
                          model.trn.rec,
                          model.trn.f1,
                          model.trn.prev,
                          model.trn.auc,
                          model.trn.cost)
  rownames(model.comp) = 1:nrow(model.comp)
  colnames(model.comp) = c("Model Type",
                           "Model Name",
                           "Accuracy",
                           "Kappa",
                           "Sensitivity",
                           "Specificity",
                           "Precision",
                           "Recall",
                           "F1",
                           "Prevalence",
                           "AUC",
                           "Cost")
  
  sink(filename, append = TRUE)
  
  cat("\n")
  print("Model engine summary")
  print("====================")
  
  print(model.comp)
  
  cat("\n")
  print("Model attribute importance")
  print("==========================")
  #Show the importance of all variables per model
  merge.all <- function(x, ..., by = "row.names") {
    L <- list(...)
    for (i in seq_along(L)) {
      x <- merge(x, L[[i]], by = by)
      rownames(x) <- x$Row.names
      x$Row.names <- NULL
    }
    return(x)
  }
  
  model.imp <- merge.all(fit.m2.imp)
  
  colnames(model.imp) = c("model")
  print(model.imp)
  
  cat("\n")
  print("Model engine results")
  print("====================")
  
  print_engine.all <- function(x, ...) {
    L <- list(...)
    for (i in seq_along(L)) {
      cat("\n")
      print("+++++++++++++")
      print(L[[i]]$method)
      print("+++++++++++++")
      print(L[[i]]$finalModel)
      cat("\n")
      print(L[[i]]$results)
    }
  }
  
  print_engine.all(fit.m2)
  
  
  sink()
  
  
}

ML_Models_ROC_P_rf <- function(training, resamp, folds, tune, r, samp, filename, imagefilename, ss = 0, summF){
  
  if(samp=="none"){
    samp <- NULL
  }
  
  #--------------------------------------
  # just run one model
  #--------------------------------------
  # Model notes:
  #   warnings() == T
  #   mtry == varied, 3 times (2,4,7)
  
  fit.m2.seeds <- setSeeds(resamp, folds, r, tune)
  
  fit.m2.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            seeds = fit.m2.seeds,
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = get(summF), #twoClassSummary,
                            sampling = samp)
  
  # Build model
  set.seed(123)
  m2.t <- system.time(fit.m2 <- train(CLASS~., data=training,
                                      method = "rf",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m2.fc,
                                      tuneLength = tune))
  runDetails(fit.m2, "fit.m2")
  
}


ML_Models_ROC <- function(training, resamp, folds, tune, r, filename){
  
  #--------------------------------------
  # Model 1 - SVM Radial
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   sigma == constant (0.547204984213807)
  #   C == varied, 3 times (0.25,0.5,1.0)
  
  # Specify fit parameters
  fit.m1.fc <- trainControl(method = resamp,
                            number = folds,
                            repeats = r,
                            classProbs = T,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m1.t <- system.time(fit.m1 <- train(CLASS~., data=training,
                                      method = "svmRadial",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m1.fc,
                                      tuneLength = tune))
  
  # In-sample summary
  fit.m1$finalModel
  fit.m1$results
  
  # Plots
  #plot(fit.m1, main = "Accuracy: fit.m1")
  #plot(varImp(fit.m1), main = "Var Imp: fit.m1")
  
  # In-sample fit
  fit.m1.trn.pred = predict(fit.m1, newdata = testing)
  fit.m1.trn.prob = predict(fit.m1, newdata = testing, type = "prob")
  fit.m1.trn.cm = confusionMatrix(fit.m1.trn.pred, testing$CLASS)
  fit.m1.trn.cm$table
  fit.m1.trn.cm$overall[1:2]
  a <- varImp(fit.m1)$importance
  a[,2] <- NULL
  fit.m1.imp <- a
  fit.m1.mRoc <- roc(testing$CLASS,fit.m1.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M1 complete")
  print(m1.t)
  sink()
  
  #--------------------------------------
  # Model 2 - Random forest
  #--------------------------------------
  # Model notes:
  #   warnings() == T
  #   mtry == varied, 3 times (2,4,7)
  
  fit.m2.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m2.t <- system.time(fit.m2 <- train(CLASS~., data=training,
                                      method = "rf",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m2.fc,
                                      tuneLength = tune))
  
  # In-sample summary
  fit.m2$finalModel
  fit.m2$results
  
  # Plots
  #plot(fit.m2, main = "Accuracy: fit.m2")
  #plot(varImp(fit.m2), main = "Var Imp: fit.m2")
  
  # In-sample fit
  fit.m2.trn.pred = predict(fit.m2, newdata = testing)
  fit.m2.trn.prob = predict(fit.m2, newdata = testing, type = "prob")
  fit.m2.trn.cm = confusionMatrix(fit.m2.trn.pred, testing$CLASS)
  fit.m2.trn.cm$table
  fit.m2.trn.cm$overall[1:2]
  a <- varImp(fit.m2)$importance
  fit.m2.imp <- a
  fit.m2.mRoc <- roc(testing$CLASS,fit.m2.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M2 complete")
  print(m2.t)
  sink()
  
  #--------------------------------------
  # Model 3 - Decision tree (J48)
  #--------------------------------------
  # Model notes:
  #   M == varied (1,2,3)
  #   C == varied (0.01, 0.255, 0.5), 3 times
  
  fit.m3.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m3.t <- system.time(fit.m3 <- train(CLASS~., data=training,
                                      method = "J48",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m3.fc,
                                      tuneLength = tune))
  
  # In-sample summary
  fit.m3$finalModel
  fit.m3$results
  
  # Plots
  #plot(fit.m3, main = "Accuracy: fit.m3")
  #plot(varImp(fit.m3), main = "Var Imp: fit.m3")
  
  # In-sample fit
  fit.m3.trn.pred = predict(fit.m3, newdata = testing)
  fit.m3.trn.prob = predict(fit.m3, newdata = testing, type = "prob")
  fit.m3.trn.cm = confusionMatrix(fit.m3.trn.pred, testing$CLASS)
  fit.m3.trn.cm$table
  fit.m3.trn.cm$overall[1:2]
  a <- varImp(fit.m3)$importance
  a[,2] <- NULL
  fit.m3.imp <- a
  fit.m3.mRoc <- roc(testing$CLASS,fit.m3.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M3 complete")
  print(m3.t)
  sink()
  
  #--------------------------------------
  # Model 4 - Bayesian Network
  #--------------------------------------
  # Model notes:
  #   no tuning
  
  fit.m4.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m4.t <- system.time(fit.m4 <- train(CLASS~., data=training,
                                      method = "bayesglm",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m4.fc))
  
  # In-sample summary
  fit.m4$finalModel
  fit.m4$results
  
  # Plots
  #plot(fit.m4, main = "Accuracy: fit.m4")
  #plot(varImp(fit.m4), main = "Var Imp: fit.m4")
  
  # In-sample fit
  fit.m4.trn.pred = predict(fit.m4, newdata = testing)
  fit.m4.trn.prob = predict(fit.m4, newdata = testing, type = "prob")
  fit.m4.trn.cm = confusionMatrix(fit.m4.trn.pred, testing$CLASS)
  fit.m4.trn.cm$table
  fit.m4.trn.cm$overall[1:2]
  a <- varImp(fit.m4)$importance
  a[,2] <- NULL
  fit.m4.imp <- a
  fit.m4.mRoc <- roc(testing$CLASS,fit.m4.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M4 complete")
  print(m4.t)
  sink()
  
  #--------------------------------------
  # Model 5 - knn
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   kmax (5,7,9)
  #   distance (fixed = 2)
  #   kernel (fixed = optimal)
  
  #   k == varied, 30 times, each = 1 (knn)
  
  # Specify fit parameters
  fit.m5.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m5.t <- system.time(fit.m5 <- train(CLASS~., data=training,
                                      method = "kknn",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m5.fc,
                                      tuneLength = tune))
  
  # In-sample summary
  fit.m5$finalModel
  fit.m5$results
  
  # In-sample fit
  fit.m5.trn.pred = predict(fit.m5, newdata = testing)
  fit.m5.trn.prob = predict(fit.m5, newdata = testing, type = "prob")
  fit.m5.trn.cm = confusionMatrix(fit.m5.trn.pred, testing$CLASS)
  fit.m5.trn.cm$table
  fit.m5.trn.cm$overall[1:2]
  a <- varImp(fit.m5)$importance
  a[,2] <- NULL
  fit.m5.imp <- a
  fit.m5.mRoc <- roc(testing$CLASS,fit.m5.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M5 complete")
  print(m5.t)
  sink()
  
  #--------------------------------------
  # Model 6 - Adaboost
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   nlter == varied, 3 times (50, 100, 150)
  #   method == varied, 2 times (Adaboost.M1, Real adaboost)
  
  # Specify fit parameters
  fit.m6.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m6.t <- system.time(fit.m6 <- train(CLASS~., data=training,
                                      method = "adaboost",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m6.fc,
                                      tuneLength = tune))
  
  # In-sample summary
  fit.m6$finalModel
  fit.m6$results
  
  # In-sample fit
  fit.m6.trn.pred = predict(fit.m6, newdata = testing)
  fit.m6.trn.prob = predict(fit.m6, newdata = testing, type = "prob")
  fit.m6.trn.cm = confusionMatrix(fit.m6.trn.pred, testing$CLASS)
  fit.m6.trn.cm$table
  fit.m6.trn.cm$overall[1:2]
  a <- varImp(fit.m6)$importance
  a[,2] <- NULL
  fit.m6.imp <- a
  fit.m6.mRoc <- roc(testing$CLASS,fit.m6.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M6 complete")
  print(m6.t)
  sink()
  
  #--------------------------------------
  # Model 7 - CART
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   cp == varied, 3 times (0.05, 0.13, 0.31)
  
  # Specify fit parameters
  fit.m7.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m7.t <- system.time(fit.m7 <- train(CLASS~., data=training,
                                      method = "rpart",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m7.fc,
                                      tuneLength = tune))
  
  # In-sample summary
  fit.m7$finalModel
  fit.m7$results
  
  # In-sample fit
  fit.m7.trn.pred = predict(fit.m7, newdata = testing)
  fit.m7.trn.prob = predict(fit.m7, newdata = testing, type = "prob")
  fit.m7.trn.cm = confusionMatrix(fit.m7.trn.pred, testing$CLASS)
  fit.m7.trn.cm$table
  fit.m7.trn.cm$overall[1:2]
  a <- varImp(fit.m7)$importance
  fit.m7.imp <- a
  fit.m7.mRoc <- roc(testing$CLASS,fit.m7.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M7 complete")
  print(m7.t)
  sink()
  
  #--------------------------------------
  # Model 8 - Neural network
  #--------------------------------------
  # Model notes:
  #   warnings() == F
  #   size
  #   decay
  
  # Specify fit parameters
  fit.m8.fc <- trainControl(method = resamp, 
                            number = folds,
                            repeats = r,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  # Build model
  set.seed(123)
  m8.t <- system.time(fit.m8 <- train(CLASS~., data=training,
                                      method = "nnet",
                                      metric = "ROC",
                                      preProcess = c("center", "scale"),
                                      trControl = fit.m8.fc,
                                      tuneLength = tune))
  
  # In-sample summary
  fit.m8$finalModel
  fit.m8$results
  
  # In-sample fit
  fit.m8.trn.pred = predict(fit.m8, newdata = testing)
  fit.m8.trn.prob = predict(fit.m8, newdata = testing, type = "prob")
  fit.m8.trn.cm = confusionMatrix(fit.m8.trn.pred, testing$CLASS)
  fit.m8.trn.cm$table
  fit.m8.trn.cm$overall[1:2]
  a <- varImp(fit.m8)$importance
  fit.m8.imp <- a
  fit.m8.mRoc <- roc(testing$CLASS,fit.m8.trn.prob[,"deceptive"], levels = c("trustworthy","deceptive"))
  
  sink(filename, append = TRUE)
  print("M8 complete")
  print(m8.t)
  sink()
  
  #--------------------------------------
  # Model Comparison
  #--------------------------------------
  
  # Model Types
  model.types = c("SVM", "tree", "tree", "bayesian", "cluster", "tree", "tree","neuralnet")
  
  # Model Names
  model.names = c("svmRadial", "rf", "J48", "bayesglm", "knn", "Adaboost", "rpart", "nnet")
  
  # Accuracy, Train
  model.trn.acc = rbind(fit.m1.trn.cm$overall[1],
                        fit.m2.trn.cm$overall[1],
                        fit.m3.trn.cm$overall[1],
                        fit.m4.trn.cm$overall[1],
                        fit.m5.trn.cm$overall[1],
                        fit.m6.trn.cm$overall[1],
                        fit.m7.trn.cm$overall[1],
                        fit.m8.trn.cm$overall[1])
  
  # Kappa, Train
  model.trn.kpp = rbind(fit.m1.trn.cm$overall[2],
                        fit.m2.trn.cm$overall[2],
                        fit.m3.trn.cm$overall[2],
                        fit.m4.trn.cm$overall[2],
                        fit.m5.trn.cm$overall[2],
                        fit.m6.trn.cm$overall[2],
                        fit.m7.trn.cm$overall[2],
                        fit.m8.trn.cm$overall[2])
  
  # Sensitivity, Train
  model.trn.sens = rbind(fit.m1.trn.cm$byClass[1],
                         fit.m2.trn.cm$byClass[1],
                         fit.m3.trn.cm$byClass[1],
                         fit.m4.trn.cm$byClass[1],
                         fit.m5.trn.cm$byClass[1],
                         fit.m6.trn.cm$byClass[1],
                         fit.m7.trn.cm$byClass[1],
                         fit.m8.trn.cm$byClass[1])
  
  # Specificity, Train
  model.trn.spec = rbind(fit.m1.trn.cm$byClass[2],
                         fit.m2.trn.cm$byClass[2],
                         fit.m3.trn.cm$byClass[2],
                         fit.m4.trn.cm$byClass[2],
                         fit.m5.trn.cm$byClass[2],
                         fit.m6.trn.cm$byClass[2],
                         fit.m7.trn.cm$byClass[2],
                         fit.m8.trn.cm$byClass[2])
  
  # Precision, Train
  model.trn.prec = rbind(fit.m1.trn.cm$byClass[5],
                         fit.m2.trn.cm$byClass[5],
                         fit.m3.trn.cm$byClass[5],
                         fit.m4.trn.cm$byClass[5],
                         fit.m5.trn.cm$byClass[5],
                         fit.m6.trn.cm$byClass[5],
                         fit.m7.trn.cm$byClass[5],
                         fit.m8.trn.cm$byClass[5])
  
  # Recall, Train
  model.trn.rec = rbind(fit.m1.trn.cm$byClass[6],
                        fit.m2.trn.cm$byClass[6],
                        fit.m3.trn.cm$byClass[6],
                        fit.m4.trn.cm$byClass[6],
                        fit.m5.trn.cm$byClass[6],
                        fit.m6.trn.cm$byClass[6],
                        fit.m7.trn.cm$byClass[6],
                        fit.m8.trn.cm$byClass[6])
  
  # F1, Train
  model.trn.f1 = rbind(fit.m1.trn.cm$byClass[7],
                       fit.m2.trn.cm$byClass[7],
                       fit.m3.trn.cm$byClass[7],
                       fit.m4.trn.cm$byClass[7],
                       fit.m5.trn.cm$byClass[7],
                       fit.m6.trn.cm$byClass[7],
                       fit.m7.trn.cm$byClass[7],
                       fit.m8.trn.cm$byClass[7])
  
  # Prevalence, Train
  model.trn.prev = rbind(fit.m1.trn.cm$byClass[8],
                         fit.m2.trn.cm$byClass[8],
                         fit.m3.trn.cm$byClass[8],
                         fit.m4.trn.cm$byClass[8],
                         fit.m5.trn.cm$byClass[8],
                         fit.m6.trn.cm$byClass[8],
                         fit.m7.trn.cm$byClass[8],
                         fit.m8.trn.cm$byClass[8])
  
  #AUC
  model.trn.auc = rbind(fit.m1.mRoc$auc,
                        fit.m2.mRoc$auc,
                        fit.m3.mRoc$auc,
                        fit.m4.mRoc$auc,
                        fit.m5.mRoc$auc,
                        fit.m6.mRoc$auc,
                        fit.m7.mRoc$auc,
                        fit.m8.mRoc$auc)
  
  #Cost
  model.trn.cost = rbind(m1.t[3],
                         m2.t[3],
                         m3.t[3],
                         m4.t[3],
                         m5.t[3],
                         m6.t[3],
                         m7.t[3],
                         m8.t[3])
  
  # Data Frame
  model.comp = data.frame(model.types,
                          model.names,
                          model.trn.acc,
                          model.trn.kpp,
                          model.trn.sens,
                          model.trn.spec,
                          model.trn.prec,
                          model.trn.rec,
                          model.trn.f1,
                          model.trn.prev,
                          model.trn.auc,
                          model.trn.cost)
  rownames(model.comp) = 1:nrow(model.comp)
  colnames(model.comp) = c("Model Type",
                           "Model Name",
                           "Accuracy",
                           "Kappa",
                           "Sensitivity",
                           "Specificity",
                           "Precision",
                           "Recall",
                           "F1",
                           "Prevalence",
                           "AUC",
                           "Cost")
  
  sink(filename, append = TRUE)
  
  cat("\n")
  print("Model engine summary")
  print("====================")
  
  print(model.comp)
  
  cat("\n")
  print("Model attribute importance")
  print("==========================")
  #Show the importance of all variables per model
  merge.all <- function(x, ..., by = "row.names") {
    L <- list(...)
    for (i in seq_along(L)) {
      x <- merge(x, L[[i]], by = by)
      rownames(x) <- x$Row.names
      x$Row.names <- NULL
    }
    return(x)
  }
  
  model.imp <- merge.all(fit.m1.imp,
                         fit.m2.imp,
                         fit.m3.imp,
                         fit.m4.imp,
                         fit.m5.imp,
                         fit.m6.imp,
                         fit.m7.imp,
                         fit.m8.imp)
  
  colnames(model.imp) = c("svmRadial", "rf", "J48", "bayesglm", "knn", "Adaboost", "rpart", "nnet")
  print(model.imp)
  
  cat("\n")
  print("Model engine results")
  print("====================")
  
  print_engine.all <- function(x, ...) {
    L <- list(...)
    for (i in seq_along(L)) {
      cat("\n")
      print("+++++++++++++")
      print(L[[i]]$method)
      print("+++++++++++++")
      print(L[[i]]$finalModel)
      cat("\n")
      print(L[[i]]$results)
    }
  }
  
  print_engine.all(fit.m1,fit.m2,fit.m3,fit.m4,fit.m5,fit.m6,fit.m7,fit.m8)
  
  sink()
  
}
