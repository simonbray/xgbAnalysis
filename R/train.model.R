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
train.model <- function(data_dir, output_dir, params, num_class, nrounds = 20) {

  if(!dir.exists(output_dir)) {
    message(paste("Creating output directory", output_dir))
    dir.create(output_dir, showWarnings = F, recursive = T)
  }

  train.matrix <- xgb.DMatrix(paste(data_dir, "train.matrix.data", sep="/"))
  test.matrix  <- xgb.DMatrix(paste(data_dir, "test.matrix.data", sep="/"))

  watchlist <- c(train = train.matrix, test = test.matrix)
  label <- fread("feature.names", header = F)[[1]]

  if(!("num_class" %in% parameter)) {
    parameter <- c(parameter, num_class = num.class)
  }

  bst <- xgb.train(data = train.matrix,
                   watchlist = watchlist,
                   params = params,
                   nrounds = nrounds)
  ##save
  xgb.save(bst, paste(output_dir, "xgb.model", sep="/"))
  xgb.dump(bst, paste(output_dir, "xgb.dump.model", sep="/"), with_stats = TRUE)
  imp <- xgb.importance(model = bst, feature_names = label)
  write.csv(imp, paste(output_dir, "importance", sep="/"))

  }
