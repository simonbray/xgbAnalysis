#' import data
#' @param crd coordinate file to import, e.g. dihedrals
#' @param sts states file to import that match coordinates
#' @param label coordinate label, as file link or 'dihedrals' for Phi2, Psi2, Phi3, ...
#' @param savefolder output folder
#' @param trainsplit how much of the data for training, rest for test
#' @import data.table
#' @import xgboost
#' @details This function reads the files \code(crd), \code(sts) and \code(label) as coordinates, states and labels.
#' The labels represent the column names of the coordinates and the states should match the rows of the coordinates.
#' The data then is separated in a training and a test set and saved in the xgboost matrix format.
#' The following files are created:
#' \itemize{
#'   \item all.xgb.Dmatrix, where the whole data set is saved.
#'   \item train.xgb.Dmatrix, where the training set is saved.
#'   \item test.xgb.Dmatrix, where the test set is saved.
#'   \item train.index, where the train indix are saved.
#'   \item test.index, where the test index is saved.
#'   \item data.import.parameter, where the input parameter are saved to use by other functions.
#' }
#' @examples
#' import.data("./file.dih", "./macrostates", "./file.dih.names", "./savefolder", 0.75)
#' import.data("./file.dih", "./macrostates", "dihedrals", "./savefolder", 0.5)
#' @export
#ToDo: savename for save prefix?

import.data <- function(crd, sts, label, savefolder, trainsplit, savename = "") {
  dir.create(savefolder, showWarnings = F, recursive = T)
  setwd(savefolder)
  
  write.csv(list(crd = crd,
                 sts = sts, 
                 label = label, 
                 trainsplit = trainsplit), 
            "import.data.parameter", 
            row.names = F,
            sep = " ")
  
  dih <- fread(crd, showProgress = F)
  if(label == "dihedrals")  {
    label <- c(paste(c("Phi", "Psi"), rep(1:(length(dih[1,])/2)+1, each = 2), sep=""))
    colnames(dih) <- label
  } else {
      label <- fread(label, showProgress = F)
      colnames(dih) <- label$V1
  }
  write.table(colnames(dih), "feature.names", row.names = F, col.names = F)
  sts <- fread(sts, showProgress = F)
  colnames(sts) <- c("states")
  num.class = max(sts[,1])
  
  #as matrix
  dih <- as.matrix(dih)
  sts <- as.matrix(sts)
  sts <- sts-matrix(data=1, nrow=nrow(sts)) #for xgboost multi:softprobe (states must begin from 0)
  
  #all data
  all.matrix <- xgb.DMatrix(data = dih, label = sts)
  xgb.DMatrix.save(all.matrix, "all.xgb.Dmatrix")
  
  #split train data
  train.index <- sample(1:nrow(dih), nrow(dih)*trainsplit)
  train.matrix <- xgb.DMatrix(data = dih[train.index,], label = sts[train.index,])
  xgb.DMatrix.save(train.matrix, "train.xgb.Dmatrix")
  
  #split test data
  if(trainsplit < 1){
    test.index <- c(1:nrow(dih))[-train.index]
    test.matrix <- xgb.DMatrix(data = dih[test.index,], label = sts[test.index,])
    xgb.DMatrix.save(test.matrix, "test.xgb.Dmatrix")
    write.table(test.index, file = "test.index", row.names = F, col.names = F)
  }
  write.table(train.index, file = "train.index", row.names = F, col.names = F)
}