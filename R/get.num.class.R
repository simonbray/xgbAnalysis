#' get number of classes
#' 
#' gets maximum class from one or two xgb.Dmatrix elements and adds 1
#' 
#' @param M1 first xgb.Dmatrix
#' @param M2 second xgb.Dmatrix
#' @import xgboost
#' @export

get.num.class <- function(M1, M2 = NA)  {
  if(is.na(M2)) {
    x <- 1 + max(getinfo(M1, "label"))
  } else {
    x <- 1 + max(c(getinfo(M1, "label"), getinfo(M2, "label")))
  }
  return(x)
}