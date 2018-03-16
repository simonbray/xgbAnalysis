#' confusion map
#'
#' makes a confusion matrix and plot for missclassification
#'
#' @param output_dir output directory
#' @param pred prediction file
#' @param average NA, 'label' or 'prediction'. Default = NA
#' @param noDiagonal should diagonal not be plotted for better visibility of missclassification? default = T
#' @importFrom data.table fread
#' @import ggplot2
#' @importFrom caret confusionMatrix
# @import plotly
# @import dplyr
#' @example
#' confusion.map(output_dir = "./confusionMap", impdata = "./data", pred = "./model/prediction", average = 'prediction', noDiagonal = F)
#' @export

confusion.map <- function(output_dir = "./confusionMap",
                          impdata    = "./data",
                          pred,
                          average    = NULL,
                          noDiagonal = T,
                          pdim       = 10,
                          width      = 1.5) {

  savename <- "cunfusion.map"
  dir.create(output_dir)
  test.index <- fread(paste(impdata, "test.index", sep = "/"), header = F)$V1
  prm <- fread(paste(impdata, "import.data.parameter", sep = "/"))
  sts <- fread(prm$states)$V1[test.index]
  pred <- 1 + fread(pred)$V1 #xgboost prediction starts with 0
  num.class <- dim(table(sts))
  label <- as.numeric(names(table(sts))[1:num.class])

  #make confusion matrix
  cm <- confusionMatrix(pred, sts)
  cm <- cm$table
  write.table(cm, paste(output_dir, "confusion.matrix", sep = "/"), row.names = F, col.names = F)

  #make average of predicted values
  if(is.null(average)){
    message("plot will not be normalized")
    }else if(average == "label"){
    for(i in 1:num.class){
      cm[i,] <- cm[i,]/sum(cm[i,])
    }
    savename <- paste(savename, average, sep = ".")
  }else if(average == "prediction"){
    for(i in 1:num.class){
      cm[,i] <- cm[,i]/sum(cm[,i])
    }
    savename <- paste(savename, average, sep = ".")
  } else {
    stop("average must be 'prediction'(=default), 'label' or NULL")
  }
  if(noDiagonal) {
    for(i in 1:num.class) {
      cm[i,i] <- 0
    }
  }
  p <- ggplot(reshape2::melt(cm)) +
    geom_raster(aes(x=melt(cm)[,2], y=melt(cm)[,1], fill=value)) +
    coord_fixed() +
    theme_bw(base_size = pdim*2) +
    labs(title = "Confusion Map",
         subtitle = paste("averaged over", average),
         x="label",
         y="prediction") +
    theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = pdim*1.8),
          plot.subtitle = element_text(size = pdim),
          axis.title.x = element_text(face = "bold"),
          axis.text.x = element_text(angle = 360),
          axis.title.y = element_text(face = "bold"),
          axis.text.y = element_text(angle = 360),
          legend.title = element_blank(),
          legend.text = element_text()
    ) +
    scale_fill_distiller(palette="Greys",
                         direction = 1) +
    guides(fill = guide_colorbar(barheight = pdim, barwidth = pdim/10)) +
    scale_x_continuous(breaks=1:num.class-1,
                       labels = label,
                       expand=c(0,0)) +
    scale_y_continuous(breaks=1:num.class-1,
                       labels = label,
                       expand = c(0,0))
  ggsave(paste(output_dir, "/", savename, ".png", sep = ""), p, width = pdim, height = pdim)

}
