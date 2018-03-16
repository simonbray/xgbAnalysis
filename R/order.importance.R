#' order importance
#'
#' order importance in dependence of population
#'
#' @param M importance matrix of model
#' @param decreasing if FALSE (default) least important feature is on top
#' @importFrom data.table fread
#' @export

order.importance <- function(M,
                             sts = NA,
                             decreasing = F)  {
  #get state population
  if(is.na(sts))  {
    pop <- table(fread(fread("./data/import.data.parameter")$states, showProgress = F))
  }else{
    pop <- table(fread(sts, showProgress = F)$V1)
  }

  #normalize
  pop <- pop/max(pop)

  #make order in dependence of population: multiplicate importance with population
  x <- order(M %*% pop, decreasing = decreasing)

  #return ordered importance
  return(M[x,])
}
