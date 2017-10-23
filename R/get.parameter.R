#' get parameter
#' 
#' opens parameter file and extracts requested parameter
#' 
# @param savefolder
#' @param parameter
#' @import data.table
#' @export

get.parameter <- function(parameter)  {
  # if(getwd() != savefolder){
  #   setwd(savefolder)
  # }
  prm <- fread("parameter")
  
  return(as.numeric(as.list(prm)[parameter]))
}
