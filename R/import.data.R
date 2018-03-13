#' Prepare training and test data.
#'
#' Reads the files \code{coords}, \code{states} and \code{labels} as coordinates,
#' states and labels. The labels represent the column names of the coordinates
#' and the states should match the rows of the coordinates. The data then is
#' separated in a training and a test set and saved in the xgboost matrix format.
#' The following files are created:
#' \itemize{
#'   \item all.xgb.Dmatrix, where the whole data set is saved.
#'   \item train.xgb.Dmatrix, where the training set is saved.
#'   \item test.xgb.Dmatrix, where the test set is saved.
#'   \item train.index, where the train indix are saved.
#'   \item test.index, where the test index is saved.
#'   \item data.import.parameter, where the input parameter are saved to be used
#'         by other functions.
#' }
#'
#' @param output_dir Character, name of the output directory.
#' @param coords Character, name of the coordinates file
#' @param states Character, name of the states file
#' @param label Character, filename or 'dihedrals' to get Phi2, Psi2, Phi3, ...
#' @param trainsplit Numeric, fraction of the data that should be used for
#'  training, in [0,1]
#' @importFrom data.table fread
#' @import xgboost
#' @return named list with elements: num_class
#' @examples
#' import.data("./savefolder","./file.dih", "./macrostates", "./file.dih.names", 0.75)
#' import.data("./savefolder", "./file.dih", "./macrostates", "dihedrals", 0.5)
#' @export
#ToDo: savename for save prefix?

import.data <- function(output_dir, coords, states, labels, trainsplit) {

  if(!dir.exists(output_dir)) {
    message(paste("Creating output directory", output_dir))
    dir.create(output_dir, showWarnings = F, recursive = T)
  }

  write.csv(list(coords = coords,
                 states = states,
                 labels = labels,
                 trainsplit = trainsplit),
            paste(output_dir, "import.data.parameter", sep="/"),
            row.names = F)

  dih <- fread(coords, showProgress = F)

  n_residues <- length(dih[1,])/2
  n_frames   <- nrow(dih)

  if(labels == "dihedrals")  {
    labels <- c(paste(c("Phi", "Psi"), rep(1:n_residues+1, each = 2), sep=""))
  } else {
    labels <- fread(labels, sep = "/", header = F)$V1
    if(substr(labels[1],1,1)=="#"){
      labels <- labels[-1]
      }
  }
  colnames(dih) <- labels
  write.table(colnames(dih),
              paste(output_dir, "feature.names", sep="/"),
              row.names = F, col.names = F)

  states <- fread(states, showProgress = F)[[1]]
  states <- factor(states)
  num.class <- length(levels(states))
  states <- as.numeric(states) - 1 #for xgboost multi:softprobe states must begin from 0

  set.parameter(paste(output_dir, "parameter", sep="/"),
                list(num_class=num.class))


  #as matrix
  dih <- as.matrix(dih)
  states <- as.matrix(states)
  colnames(states) <- c("states")

  #all data
  all.matrix <- xgb.DMatrix(data = dih, label = states)
  xgb.DMatrix.save(all.matrix,
                   paste(output_dir, "all.xgb.Dmatrix", sep="/"))

  #split train data
  train.index  <- sample(1:nrow(dih), nrow(dih)*trainsplit)
  train.matrix <- xgb.DMatrix(data = dih[train.index,], label = states[train.index,])

  xgb.DMatrix.save(train.matrix,
                   paste(output_dir,"train.xgb.Dmatrix", sep="/"))
  write.table(train.index,
              paste(output_dir, "train.index", sep="/"),
              row.names = F, col.names = F)

  #split test data
  if(trainsplit < 1){
    test.index  <- c(1:nrow(dih))[-train.index]
    test.matrix <- xgb.DMatrix(data = dih[test.index,], label = states[test.index,])

    xgb.DMatrix.save(test.matrix,
                     paste(output_dir, "test.xgb.Dmatrix", sep="/"))
    write.table(test.index,
                paste(output_dir, "test.index", sep="/"),
                row.names = F, col.names = F)
  }
  return(list(num_class=num.class))
}
