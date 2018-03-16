#' cross validation
#'
#' calculates cross validation for given parameter
#'
# @param savefolder
#' @param nthread number of CPU cores to use, NA for all
#' @param nfold how many folds of dataset
#' @param nrounds number of training rounds
#' @param defpar default parameter:default = list(eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
#' @param testparname
#' @param testpar
#' @param plot should results be plotted, default=TRUE
#' @param pdim dimension of plot, default=10
#' @param width proportion of width to height, default=1.5
#' @import data.table
#' @import ggplot2
#' @import xgboost
#' @export
#ToDo: pre, all.matrix.data needed

cross.validation <- function(#savefolder,
                             pre = "",
                             nfold,
                             nthread,
                             nrounds,
                             testparname = NA,
                             testpar = NA,
                             plot = T,
                             pdim = 10,
                             width = 1.5)  {
  #setwd(savefolder)
  check.matrix()
  dir.create("cross.validation")
  all.matrix <- xgb.DMatrix("all.matrix.data")
  parameter <- as.list(fread("parameter"))

  if(!is.na(testpar)) {
    parameter[testparname] <- testpar
  }


  cv_model <- xgb.cv(
    params = parameter,
    data = all.matrix,
    nrounds = nrounds,
    nfold = nfold,
    nthread = nthread,
    showsd = F,
    verbose = T,
    prediction = T
  )
  fwrite(data.frame(train = cv_model$evaluation_log$train_merror_mean,
                    test = cv_model$evaluation_log$test_merror_mean),
         paste(pre, "cv.model_nrounds", nrounds, "_nfold", nfold, ".error", sep = ""),
         col.names = T,
         row.names = F)

  p <- ggplot(cv_model$evaluation_log, aes(cv_model$evaluation_log$iter, y = value, color = variable)) +
    geom_point(aes(y = cv_model$evaluation_log$test_merror_mean, col = "test-merror")) +
    geom_point(aes(y = cv_model$evaluation_log$train_merror_mean, col = "train-merror"))+
    theme_bw(base_size = pdim*2) +
    labs(title = "Cross Validation",
         x="nround",
         y="error") +
    theme(plot.title = element_text(face = "bold.italic", hjust = 0.5),
          axis.title.x = element_text(face = "bold"),
          axis.text.x = element_text(),
          axis.title.y = element_text(face = "bold"),
          axis.text.y = element_text(),
          legend.title = element_blank(),
          legend.text = element_text(),
          legend.justification = c(1, 1),
          legend.position = c(1, 1),
          legend.background = element_rect(colour = "black")
    )

  ggsave(filename = paste(pre, "cv.model_nrounds", nrounds, "_nfold", nfold, ".png", sep = ""),
         p, width = pdim*width, height = pdim)
}


