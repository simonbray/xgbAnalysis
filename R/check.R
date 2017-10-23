#' check for import parameter
#'
#' @export

check.import.parameter <- function()  {
  if(!file.exists("import.data.parameter"))  {
    stop("Import parameter have to be set in 'import.data.parameter'. Run 'import.data' as first step!")
  }
}

#' check for parameter
#' 
#' @export

check.parameter <- function() {
  if(!file.exists("parameter"))  {
    stop("Parameter have to be set in 'parameter'. Run 'set.parameter'!")
  }
}

#' check for xgb.Dmatrix
#' 
#' @export

check.matrix <- function() {
  if(!file.exists("all.xgb.Dmatrix"))  {
    stop("xgb.Dmatrix not found. Run 'import.data' as first step!")
  }
}