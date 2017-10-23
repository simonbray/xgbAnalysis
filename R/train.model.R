#' train model
#' 
#' trains xgboost model
#'
#' @param savefolder
#' @param nrounds
#' @import xgboost
#' @import data.table
#' @export
#ToDo: Parameter, test.matrix
train.model <- function(savefolder,
                        nrounds = 20) {
  setwd(savefodler)
  dir.create("model")
  parameter <- as.list(fread(parameter))
  train.matrix <- xgb.DMatrix("train.matrix.data")
  test.matrix <- xgb.DMatrix("test.matrix.data")
  watchlist <- c(train = train.matrix, test = test.matrix)
  num.class <- get.num.class(train.matrix, test.matrix)
  label <- fread("feature.names", header = F)$V1
  
  if(!("eval_metric" %in% parameter)) {
    parameter <- c(parameter, eval_metric = 'merror')
  }
  if(!("objective" %in% parameter)) {
    parameter <- c(parameter, objective = 'multi:softmax')
  }
  if(!("num_class" %in% parameter)) {
    parameter <- c(parameter, num_class = num.class)
  }
  if(!("nthread" %in% parameter)) {
    parameter <- c(parameter, nthread = 0)
  }
  if(!("eta" %in% parameter)) {
    parameter <- c(parameter, eta = 0.3)
  }
  if(!("max_depth" %in% parameter)) {
    parameter <- c(parameter, max_depth = 6)
  }
  
  bst <- xgb.train(data = train.matrix, 
                   watchlist = watchlist, 
                   params = parameter,
                   nrounds = nrounds
                   )
  ##save
  xgb.save(bst, paste("model/xgb.model", sep=""))
  xgb.dump(bst, paste("model/xgb.dump.model", sep=""), with_stats = TRUE)
  imp <- xgb.importance(model = bst, feature_names = label)
  write.csv(imp, "model/importance")
  
  }