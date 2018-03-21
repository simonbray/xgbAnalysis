#' Feature Selection.
#'
#' Dismisses features iteratively by their importance and monitor classification
#' accuracy. \cr
#' Trains iteratively a xgb model while dismissing features by importance and
#' monitoring accuracy. Decreasing will dismiss the most important feature each
#' round, increasing (default) the least important. The file 'feature.importance'
#' will contain an ordered list of all dismissed features. \cr
#' The parameter values used for feature selection are written to the file
#' 'parameter'.
#' Additionally, in savemode = T, the following files will be created each round i:
#' \itemize{
#'   \item round<i>.model, where the trained model is saved.
#'   \item round<i>.prediction, where the predictions are saved.
#'   \item round<i>.importance, where the feature importanec is saved.
#' }
#'
#' @param output_dir output directory
#' @param data path to data directory \link{\code{import.data}}
#' @param decreasing decreasing = F :least important feature will be dismissed
#' @param ndismiss how many features (coordinates) should be dismissed? Use just when \code{decreasing=TRUE}
#' @param fdismissed coordinate names that should be dismissed, before starting to remove coordinates based on feature importance. E.g. fdismissed = c("Phi2","Psi10")
#' @param nrounds XGBoost parameter: training rounds
#' @param nthread 0 for all
#' @param eta XGBoost parameter: learning rate; if NA, parameter from train.data file in data directory will be taken
#' @param max_depth XGBoost parameter: maximum tree depth; if NA, parameter from train.data file in data directory will be taken
#' @param savemode save xgb models (T/F)
#' @import xgboost
#' @import data.table
#' @example
#' feature.selection(output_dir = "/featureSelection", data = "data/import.data.parameter", decreasing = F, eta = 0.3, max_depth = 10, nrounds = 20, nthread = 12, savemode = T)
#' @export
#ToDo: exists testsplit:rearrange accuracy -> take testset for granted?!

feature.selection <- function(output_dir = "./featureSelection",
                              data       = "./data",
                              decreasing = F,
                              ndismiss   = NA,
                              fdismissed  = NA,
                              nrounds    = 20,
                              nthread    = 0,
                              eta        = NA,
                              max_depth  = NA,
                              savemode   = T
                              ) {

  # check.import.parameter()

  prm <- fread(paste(data, "import.data.parameter", sep = "/"))
  crd <- prm$coords
  sts <- prm$states
  label <- prm$labels

  if(is.na(eta)){
    eta <- get.parameter(params    = paste(data, "train.parameter", sep = "/"),
                         parameter = "eta")
  }
  if(is.na(max_depth)){
    max_depth <- get.parameter(params    = paste(data, "train.parameter", sep = "/"),
                               parameter = "max_depth")
  }
  # if(decreasing){
  #  output_dir <- "/feature.selection/decreasing"
  # } else {
  #  output_dir <- "/feature.selection/increasing"
  # }
  dir.create(output_dir, showWarnings = F)

  write.csv(list(decreasing = decreasing,
                 eta = eta,
                 max_depth = max_depth,
                 nrounds = nrounds,
                 nthread = nthread),
            paste(output_dir, "parameter", sep = "/"),
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

  if(!is.na(fdismissed[1])){
    dih <- dih[,-which(label %in% fdismissed)]
    message(paste("the coordinates ", paste(fdismissed, collapse = " "), " have been removed from the training data...", sep = ""))
  }

  #split train data
  train.index <- fread(paste(data, "train.index", sep = "/"), header = F)$V1
  #train.matrix <- xgb.DMatrix(data = dih[train.index,], label = sts[train.index,])

  #split test data
  if(file.exists(paste(data, "test.index", sep = "/"))) {
    test.index <- fread(paste(data, "test.index", sep = "/"), header = F)$V1
    #test.matrix <- xgb.DMatrix(data = dih[test.index,], label = sts[test.index,])
    test.label <- sts[test.index]
    # watchlist <- c(train = train.matrix, test = test.matrix)
  }
  # else {
  #   watchlist <- c(train = train.matrix)
  # }

  if(!is.na(ndismiss)){
    selectrounds <- ndismiss-1
    message(paste("ndismiss = ", ndismiss, ": the first/last ", ndismiss, "coordinates will be dismissed...", sep = ""))
  } else {
    selectrounds <- dim(dih)[2]-1
  }
  M <- matrix(nrow = 1 + selectrounds, ncol = 3 + num.class)
  M <- as.data.frame(M)
  colnames(M) <- c("round", "feature dismissed", "accuracy", 1:num.class)
  if(!is.na(fdismissed[1])){
    M[1:length(fdismissed),1] <- 0:(length(fdismissed)-1)
    M[1:length(fdismissed),2] <- fdismissed
    start <- length(fdismissed)
    message(paste("starting from selectround ", start, "...", sep = ""))
  } else {
    start <- 0
  }

  for(i in start:selectrounds) {
    ##xgboost
    message(paste("selectround ", i, ": start training model..." ,sep = ""))
    if(is.null(dim(dih))){
      dih <- as.matrix(dih)
      colnames(dih) <- as.character(imp[2,1])
    }
    train.matrix <- xgb.DMatrix(data = as.matrix(dih[train.index,]), label = sts[train.index,])
    if(exists("test.index")) {
      test.matrix <- xgb.DMatrix(data = as.matrix(dih[test.index,]), label = sts[test.index,])
      watchlist <- list(train = train.matrix, test = test.matrix)
    } else {
      watchlist <- list(train = train.matrix)
    }
    # if(exists("test.index")) {#if(file.exists("data/test.index")) {
    #   bst <- xgb.train(data = xgb.DMatrix(data = dih[train.index,], label = sts[train.index,]),#train.matrix,
    #                    watchlist = list(train = xgb.DMatrix(data = dih[train.index,], label = sts[train.index,]),
    #                                     test  = xgb.DMatrix(data = dih[test.index,], label = sts[test.index,])),#watchlist,
    #                    eval_metric = 'merror',
    #                    objective = 'multi:softmax',
    #                    num_class = num.class,
    #                    eta = eta,
    #                    nrounds = nrounds,
    #                    max_depth = max_depth,
    #                    nthread = nthread)
    # } else {
      bst <- xgb.train(data = train.matrix,#xgb.DMatrix(data = dih[train.index,], label = sts[train.index,]),#train.matrix,
                       watchlist = watchlist,#list(train = xgb.DMatrix(data = dih[train.index,], label = sts[train.index,])),#
                       eval_metric = 'merror',
                       objective = 'multi:softmax',
                       num_class = num.class,
                       eta = eta,
                       nrounds = nrounds,
                       max_depth = max_depth,
                       nthread = nthread)
    # }

    message("ready...")
    M[i+1,1] <- i
    pred <- predict(bst, xgb.DMatrix(data = as.matrix(dih[test.index,]), label = sts[test.index,]))#test.matrix)
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
    write.table(M, paste(output_dir, "/feature.selection", sep = ""), row.names = F)
    message(paste("selectround ", i, ": ", as.character(imp[1,1]), " dismissed from data set..." ,sep = ""))
    if(savemode)  {
      message(paste("selectround ", i, ": save model and stats..." ,sep = ""))
      xgb.save(bst, paste(output_dir, "/selectround", i, ".model", sep=""))
      write.table(imp, file = paste(output_dir, "/selectround", i, ".importance", sep = ""))
      write.table(cbind("prediction" = pred), file = paste(output_dir, "/selectround", i, ".prediction", sep = ""), row.names = F, col.names = F)
    }
    if(i<selectrounds){
      dih <- dih[,colnames(dih)!=as.character(imp[1,1])]
      # if(is.null(dim(dih))){
      #   dih <- as.matrix(dih)
      #   colnames(dih) <- as.character(imp[2,1])
      # }
      # train.matrix <- xgb.DMatrix(data = as.matrix(dih[train.index,]), label = sts[train.index,])
      # if(file.exists("data/test.index")) {
      #   test.matrix <- xgb.DMatrix(data = as.matrix(dih[test.index,]), label = sts[test.index,])
      #   watchlist <- list(train = train.matrix, test = test.matrix)
      # } else {
      #   watchlist <- list(train = train.matrix)
      # }
    }
  }
}
