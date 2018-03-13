#' accuracy loss data
#' 
#' dismisses features iteratively by importance and monitors accuracy
#' 
#' @param dir
#' @param decreasing decreasing = F :least important feature will be dismissed
#' @param eta
#' @param nrounds
#' @param max_depth
#' @param nthread 0 for all
#' @param savemode save xgb models (T/F)
#' @import xgboost
#' @import data.table
#' @example 
#' feature.selection(dir = "/featureSelection", decreasing = F, eta = 0.3, max_depth = 10, nrounds = 20, nthread = 12, savemode = T)
#' @details 
#' Trains iteratively a xgb model while dismissing features by importance and monitoring accuracy.
#' Decreasing will dismiss the most important feature each round, increasing (default) the least important.
#' The file 'feature.importance' will contain an ordered list of all dismissed features. The param
#' Additionally, in savemode = T, the following files will be created each round \italic{i}:
#' \itemize{
#'   \item roundi.model, where the trained model is saved.
#'   \item roundi.prediction, where the predictions are saved.
#'   \item roundi.importance, where the feature importanec is saved.
#' }
#' @export
#ToDo: exists testsplit:rearrange accuracy; set new parameter (eta,max_depth) or take from parameter file?

feature.selection <- function(dir = "/featureSelection",
                              decreasing = F,
                              eta = 0.3,
                              max_depth = 6,
                              nrounds = 20,
                              nthread = 0,
                              savemode = T) {
  
  check.import.parameter()
  
  prm <- fread("import.data.parameter")
  crd <- prm$crd
  sts <- prm$sts
  label <- prm$label
  
  # if(decreasing){
  #   dir <- "/feature.selection/decreasing"
  # } else {
  #   dir <- "/feature.selection/increasing"
  # }
  dir.create(dir, showWarnings = F)
  
  write.csv(list(decreasing = decreasing,
                 eta = eta, 
                 max_depth = max_depth,
                 nrounds = nrounds,  
                 nthread = nthread), 
            paste(dir, "parameter", sep = "/"), 
            row.names = F)
  
  impfeature = 'Gain' #change if neccessary
  
  dih <- fread(crd)
  if(label == "dihedrals")  {
    label <- c(paste(c("Phi", "Psi"), rep(1:(length(dih[1,])/2)+1, each = 2), sep=""))
  } else {
    #read label from file
    label <- fread(label, sep = "/", header = F)$V1
    #remove first line, if comment
    if(substr(label[1],1,1)=="#"){
      label <- label[-1]
    }
  }
  colnames(dih) <- label
  sts <- fread(sts)
  colnames(sts) <- c("states")
  num.class = max(sts[,1])
  
  #as matrix
  dih <- as.matrix(dih)
  sts <- as.matrix(sts)
  sts <- sts-matrix(data=1, nrow=nrow(sts)) #for xgboost multi:softprobe (states must begin from 0)

  #split train data
  train.index <- fread("train.index", header = F)$V1
  train.matrix <- xgb.DMatrix(data = dih[train.index,], label = sts[train.index,])
  
  #split test data
  if(file.exists("test.index")) {
    test.index <- fread("test.index", header = F)$V1
    test.matrix <- xgb.DMatrix(data = dih[test.index,], label = sts[test.index,])
    test.label <- sts[test.index]
    watchlist <- c(train = train.matrix, test = test.matrix)
  } else {
    watchlist <- c(train = train.matrix)
  }

  selectrounds <- dim(dih)[2]-1
  
  M <- matrix(nrow = 1 + selectrounds, ncol = 3 + num.class)
  M <- as.data.frame(M)
  colnames(M) <- c("round", "feature dismissed", "accuracy", 1:num.class)
  
  for(i in 0:selectrounds) {
    ##xgboost
    bst <- xgb.train(data = train.matrix, 
                     watchlist = watchlist,
                     eval_metric = 'merror',
                     objective = 'multi:softmax',
                     num_class = num.class,
                     eta = eta,
                     nrounds = nrounds,
                     max_depth = max_depth,
                     nthread = nthread)
    M[i+1,1] <- i
    pred <- predict(bst, test.matrix)
    M[i+1, 3] <- round(sum(pred == test.label)/(length(pred)), 4)
    for(j in 0:(num.class-1)){
      M[i+1,j+4] <- round(sum((test.label == j) & (pred == j))/sum(test.label == j) -
                            sum((pred == j) & (test.label != j))/length(test.label), 4)
    }
    
    feature.names <- colnames(dih)
    ##save

    imp <- xgb.importance(model = bst, feature_names = feature.names)
    if(impfeature == 'Gain'){
      imp <- imp[sort.list(imp$Gain, decreasing = decreasing),]
    } else if( impfeature == 'Cover'){
      imp <- imp[sort.list(imp$Cover, decreasing = decreasing),]
    } else if( impfeature == 'Frequency'){
      imp <- imp[sort.list(imp$Frequency, decreasing = decreasing),]
    }
    M[i+1,2] <- as.character(imp[1,1])
    write.table(M, paste(dir, "/feature.selection", sep = ""), row.names = F)
    if(savemode)  {
      xgb.save(bst, paste(dir, "/selectround", i, ".model", sep=""))
      write.table(imp, file = paste(dir, "/selectround", i, ".importance", sep = ""))
      write.table(cbind("prediction" = pred), file = paste(dir, "/selectround", i, ".prediction", sep = ""), row.names = F, col.names = F)
    }
    if(i<selectrounds){
      dih <- dih[,colnames(dih)!=as.character(imp[1,1])]
      if(is.null(dim(dih))){
        dih <- as.matrix(dih)
        colnames(dih) <- as.character(imp[1,1])
      }
      train.matrix <- xgb.DMatrix(data = as.matrix(dih[train.index,]), label = sts[train.index,])
      if(file.exists("test.index")) {
        test.matrix <- xgb.DMatrix(data = as.matrix(dih[test.index,]), label = sts[test.index,])
        watchlist <- list(train = train.matrix, test = test.matrix)
      } else {
        watchlist <- list(train = train.matrix)
      }
    }
  }
}
