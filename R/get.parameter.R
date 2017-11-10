#' get parameter
#' 
#' opens parameter file and returns requested parameter
#' 
#' @param parameter
#' @import data.table
#' @export

get.parameter <- function(parameter)  {
  prm <- fread("parameter")
  return(as.numeric(as.list(prm)[parameter]))
}
