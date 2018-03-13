#' single class importance
#' 
#' calculates importance of single class from xgb.model
#'
#' @param x class for which importance should be calculated
#' @param label path to feature names
#' @import xgboost
#' @import data.table
#' @export

single.class.importance <- function(model = NA,
                                    names = NA,
                                    x)  {
  
  if(is.na(model))  {
    bst <- xgb.load("model/xgb.model")
  } else {
    bst <- xgb.load(model)
  }
  if(is.na(names))  {
    label <- fread("feature.names", header = F)$V1
  } else {
    label <- fread(names, sep = "/", header = F)$V1
    if(substr(label[1],1,1)=="#"){
      label <- label[-1]
    }
  }

  
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
