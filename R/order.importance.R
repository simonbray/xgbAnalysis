#' order importance
#' 
#' order importance in dependence of population
#'
#' @param M importance matrix of model
#' @param decreasing if FALSE (default) least important feature is on top
#' @import data.table
#' @export
 
order.importance <- function(M,
                             decreasing = F)  {
  #get state population
  pop <- table(fread(fread("import.data.parameter")$sts, showProgress = F))
  
  #normalize
  pop <- pop/max(pop)
  
  #make order in dependence of population: multiplicate importance with population
  x <- order(M %*% pop, decreasing = decreasing)
  
  #return ordered importance
  return(M[x,])
}
