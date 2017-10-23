#' cross validation parameter test
#' 
#' calculates cross validation for given parameter and a testparameter
#' 
# @param savefolder
#' @param testpar e.g. list(eta = 0.1, 0.2, ...)
#' @param nthread number of CPU cores to use, NA for all
#' @param nfold how many folds of dataset
#' @param nrounds number of training rounds
#' @param plot should results be plotted, default=TRUE
#' @param pdim dimension of plot, default=10
#' @param width proportion of width to height, default=1.5
#' @import data.table
#' @import ggplot2
#' @import xgboost
#' @export
 
cv.parameter.test <- function(#savefolder,
                              testpar,
                              nfold,
                              nthread,
                              nrounds = 200,
                              plot = T,
                              pdim = 10,
                              width = 1.5)  {
  #setwd(savefolder)
  dir.create("cv.parameter.test")
  for(i in 1:length(testpar)) {
    prm <- as.data.frame(testpar[i])
    for(j in 1:dim(prm)[1])  {
      cross.validation(#savefolder,
                       pre = paste("cv.parameter.test/", names(prm), prm[j,1], "_", sep = ""),
                       nfold,
                       nthread,
                       nrounds,
                       testparname = names(prm),
                       testpar = prm[j,1],
                       plot,
                       pdim,
                       width)
    }
  }
  
  
}
