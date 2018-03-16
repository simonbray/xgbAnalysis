#' Train xgboost model.
#'
#' TODO: detailed description.
#' TODO: Parameter, test.matrix (?)
#' TODO: data_dir
#'
#' @param data_dir directory containing the files train.parameter,
#' feature.names, train.xgb.Dmatrix, test.xgb.Dmatrix (see \link{\code{import.data}})
#' @param output_dir directory to which results are written
#' @param nrounds number of training rounds
#' @import xgboost
#' @importFrom data.table fread
#' @export

train.model <- function(data_dir, output_dir, params=NA, nrounds = 20) {

  # check if output dir exists and create it if not
  if(!dir.exists(output_dir)) {
    message(paste("Creating output directory", output_dir))
    dir.create(output_dir, showWarnings = F, recursive = T)
  }
  if(is.na(params)){
    params <- fread(paste(data_dir, "train.parameter", sep="/"))
  }

  # read train and test data
  message("Reading training and test data ...")
  train.matrix <- xgb.DMatrix(paste(data_dir, "train.xgb.Dmatrix", sep="/"))
  test.matrix  <- xgb.DMatrix(paste(data_dir, "test.xgb.Dmatrix", sep="/"))
  message("... finished.")

  watchlist <- c(train = train.matrix, test = test.matrix)
  label <- fread(paste(data_dir, "feature.names", sep="/"), header = F)[[1]]

  # TODO check if num_class is correct?

  message("Training xgboost model ...")
  bst <- xgb.train(data = train.matrix,
                   watchlist = watchlist,
                   params = params,
                   nrounds = nrounds)
  message("... finished.")

  # Save results, TODO: message(...) which files are stored where
  xgb.save(bst, paste(output_dir, "xgb.model", sep="/"))
  xgb.dump(bst, paste(output_dir, "xgb.dump.model", sep="/"), with_stats = TRUE)
  imp <- xgb.importance(model = bst, feature_names = label)
  write.csv(imp, paste(output_dir, "importance", sep="/"))
}
