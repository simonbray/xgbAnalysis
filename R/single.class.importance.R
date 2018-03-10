#' Single Class Importance
#'
#' Calculate the importance of a single class from a given xgb Model.
#'
#' @param model_fn name of the file storing the xgb model
#'  (see \link{train.model})
#' @param feature_names_fn name of the file storing the feature names
#'  (see \link{import.data})
#' @param x class for which importance should be calculated
#' @import xgboost
#' @importFrom data.table fread
#' @export

single.class.importance <- function(model_fn, feature_names_fn, x)  {

  bst <- xgb.load(model_fn)
  label <- fread(feature_names_fn, header = F)$V1

  dt <- xgb.model.dt.tree(label, bst)
  num.class <- get.parameter("num_class")
  nrounds <- (max(dt[,1]) + 1)/num.class

  #creating list with corresponding tree indices
  xlist <- list()
  for(i in 1:nrounds){#i is class
    xlist[i] <- x-1+(i-1)*num.class
  }

  #getting new data table with corresponding indices
  dt_new <- dt[as.matrix(dt[,1]) %in% xlist,]
  dt_new[,1] <- (dt_new[,1]-dt_new[,1]%%num.class)*2/num.class + dt_new[,1]%%num.class

  #gain
  gain <- c()
  gain[label] <- 0
  for(i in 1:length(as.matrix(dt_new[,1]))){
    if(dt_new$Feature[i] %in% label){
      gain[c(dt_new$Feature[i])] <- gain[c(dt_new$Feature[i])] + dt_new$Quality[i]
    }
  }

  Gain <- gain/sum(gain)

  #frequency
  freq <- c()
  freq[label] <- 0
  for(i in 1:length(freq)){
    freq[i] <- sum(dt_new$Feature == label[i])
  }
  Frequency <- freq/sum(freq)

  #cover
  cover <- c()
  cover[label] <- 0
  for(i in 1:length(as.matrix(dt_new[,1]))){
    if(dt_new$Feature[i] %in% label){
      cover[c(dt_new$Feature[i])] <- cover[c(dt_new$Feature[i])] + dt_new$Cover[i]
    }
  }
  Cover <- cover/sum(cover)

  importance <- cbind(Gain, Cover, Frequency)
  importance <- importance[order(-Gain),]
  return(importance)
}
