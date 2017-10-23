#' set parameter
#' 
#' @param parameter list of parameters, e.g. list(eta = 0.3, max_depth = 6, ...)
#' @import data.table
#' @export

set.parameter <- function(parameter)  {
  defpar = list(objective = "multi:softmax",
                num_class = 2,
                eta= 0.3,
                gamma=0,
                max_depth=6,
                min_child_weight=1,
                subsample=1,
                colsample_bytree=1)
  for(i in names(parameter))  {
    defpar[i] <- parameter[i]
  }
  fwrite(defpar, "parameter")
}