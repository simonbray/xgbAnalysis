#' single class importance plotter
#' 
#' plots importance from single.class.importance
#'
# @param savefolder
#' @param model directory to xgb.model file. If NA (default) model from ./model/xgb.model is taken.
#' @param colmin Sets minimum of imnportance a feature has to contribute to at least one class. Can be set as vector to produce multiple plots: colmin = c(0.1,0.2,0.3)
#' @param nfeatures Returns plot with 'nfeatures' most important features. Can be set as vector to produce multiple plots: c(4,5,6)
#' @param pdim plot dimension (height = pdim)
#' @param width plot width (pdim*width)
#' @import ggplot2
#' @import data.table
#' @export

plt.single.class.importance <- function(pre = "singleClassImportance/sci",
                                        model = NA,
                                        colmin = NA,
                                        nfeatures = NA,
                                        pdim = 10,
                                        width = 1)  {
  
  dir.create(dirname(pre), showWarnings = F)
  label <- fread("feature.names",showProgress = F ,header = F)$V1
  num.class <- get.parameter("num_class")
  impfeature = "Gain" #change if neccessary to 'Cover' or 'Frequency'
  
  #label <- rev(label)
  M <- matrix(0, ncol = num.class, nrow = length(label))
  colnames(M) <- c(1:num.class)
  rownames(M) <- rev(label)
  
  for(i in 1:num.class) {
    sci <- single.class.importance(model = model, i)
    M[label,i] <- sci[label,impfeature]
  }
  write.csv(M, paste(pre, "_data", sep = ""))
  if(!is.na(colmin[1]))  {
    for(i in 1:length(colmin))  {
      plt.sci(M[M[cbind(c(1:length(label)), max.col(M))]>colmin[i],],
              paste(pre, "_colmin", colmin[i], sep = ""),
              pdim,
              width)
      }
  }
  if(!is.na(nfeatures[1])) {
    M <- order.importance(M, decreasing = T)
    for(i in 1:length(nfeatures)) {
      plt.sci(M[nfeatures[i]:1,],
              paste(pre, "_n", nfeatures[i], sep = ""),
              pdim,
              width)
    }
  }

}
