#' parameter test
# @param savefolder
#' @param nthread number of CPU cores to use, NA for all
#' @param nrounds number of training rounds
#' @param defpar default parameter, default = list(eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
#' @param testpar list of parameter to test: data.frame(parameter name = c(start, stop, stepsize, adjust nrounds(T/F)), eta = c(0.1,0.5,0.1,T))
#' @param plot should results be plotted, default=TRUE
#' @param pdim dimension of plot, default=10
#' @param width proportion of width to height, default=1.5
#' @import data.table
#' @import ggplot2
#' @import xgboost
#' @example 
#' parameter.test(nthread = 12, nrounds = 20, testpar = data.frame(eta = c(0.1, 0.5, 0.1, T), max_depth = c(3, 10, 1, T)))
#' @export

parameter.test <- function(#savefolder,
                           nthread,
                           nrounds,
                           defpar = list(objective = "multi:softmax",
                                         num_class = 2,
                                         eta= 0.3,
                                         gamma=0,
                                         max_depth=6,
                                         min_child_weight=1,
                                         subsample=1,
                                         colsample_bytree=1),
                           testpar,
                           plot=T,
                           pdim=10,
                           width=1.5) {

  train.matrix <- xgb.DMatrix("train.matrix.data")
  test.matrix <- xgb.DMatrix("test.matrix.data")
  watchlist <- c(train = train.matrix, test = test.matrix)
  
  dir.create("/partest")
  dir <- "/partest"
  
  #num.class <- get.num.class(train.matrix, test.matrix)
  defpar$num_class <- get.num.class(train.matrix, test.matrix)
  
  #loop for every parameter
  for(i in 1:(dim(testpar)[2])){
    #load parameter name and values
    pm <- colnames(testpar)[i]
    p1 <- testpar[1,i] #start
    p2 <- testpar[2,i] #stop
    p3 <- testpar[3,i] #stepsize
    p4 <- testpar[4,i] #nrounds adjustment
    steps <- round((p2-p1)/p3)
    
    #matrix for evaluation
    parm <- matrix(nrow = steps+1, ncol = 3)
    colnames(parm) <- c(pm, "nrounds", "accuracy")
    #loop for parameter values
    for(j in 0:steps){
      #load change in default parameters
      params <- defpar
      params[pm] <- p1 + j*p3
      #write parameter in eval.m.
      parm[j+1,1] <- p1 + j*p3
      #train model
      if(p4 == 1){#adjust nrounds
        nroundsave <- nrounds
        nrounds <- round(nrounds*p2/as.numeric(parm[j+1,1]))
      }
      bst <- xgb.train(data = train.matrix,
                       watchlist = watchlist, 
                       params = params,
                       nrounds = nrounds, 
                       nthread = nthread)
      #make prediction based on model
      pred <- predict(bst, test_data)
      
      #save accuracy; rewrites file every round. one file per parameter
      parm[j+1,2] <- nrounds
      nrounds <- nroundsave
      parm[j+1,3] <- sum(pred == test_label)/(length(pred))
      
      
      write.csv(parm, file = paste(dir, "/partest_", pm, sep = ""))
    }
    #make plot if plot TRUE
    if(plot){
      #make dataframe for ggplot
      parm <- as.data.frame(parm)
      #make plot
      p <- ggplot(parm, aes(parm[,pm], y = value)) + 
        geom_point(aes(y = parm[,"accuracy"])) +
        geom_line(aes(y = parm[,"accuracy"])) +
        theme_bw(base_size = pdim*2) +
        labs(title = paste("Accuracy in dependency of", pm),
             x= pm, 
             y="accuracy") +
        theme(plot.title = element_text(face = "bold.italic", hjust = 0.5),
              axis.title.x = element_text(face = "bold"),
              axis.text.x = element_text(),
              axis.title.y = element_text(face = "bold"),
              axis.text.y = element_text()
        ) +
        scale_x_continuous(breaks=0:(steps)*p3)
      if(p4 == 1) {#info for adjusted nrounds for eta
        p <- p + labs(subtitle = "iterations = nrounds*(max_value/current_value)") + 
                theme(plot.subtitle = element_text(size = pdim))
      }
      #save plot
      ggsave(filename = paste(dir, "/partest_", pm, ".png", sep = ""),
             p, width = pdim*width, height = pdim)
      
    }
  }
}