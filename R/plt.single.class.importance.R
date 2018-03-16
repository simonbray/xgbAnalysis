#' Single class importance plot
#'
#' Plot the feature importance per state. Feature importance is normalized. \cr
#' Specify \code{colmin} to obtain a plot containing all coordinates with
#' a feature importance larger than this minimum importance. \cr
#' Specify \code{nfeatures} to obtain a plot containing the n most important
#' coordinates.
#' TODO update parameter descriptions
#'
#' @param model directory to xgb.model file. If NA (default) model from ./model/xgb.model is taken.
#' @param colmin Sets minimum of imnportance a feature has to contribute to at least one class. Can be set as vector to produce multiple plots: colmin = c(0.1,0.2,0.3)
#' @param nfeatures Returns plot with 'nfeatures' most important features. Can be set as vector to produce multiple plots: c(4,5,6)
#' @param pdim plot dimension (height = pdim)
#' @param width plot width (pdim*width)
#' @import ggplot2
#' @importFrom data.table fread
#' @export

plt.single.class.importance <- function(pre = "singleClassImportance/sci",
                                        model = NA,
                                        names = NA,
                                        colmin = NA,
                                        nfeatures = NA,
                                        pdim = 10,
                                        width = 1)  {

  dir.create(dirname(pre), showWarnings = F)
  if(is.na(names))  {
    label <- fread("feature.names", header = F)$V1
  } else {
    label <- fread(names, sep = "/", header = F)$V1
    if(substr(label[1],1,1)=="#"){
      label <- label[-1]
    }
  }
  num.class <- get.parameter(params="./data/train.parameter",
                             parameter="num_class")
  impfeature = "Gain" #change if neccessary to 'Cover' or 'Frequency'

  #label <- rev(label)
  M <- matrix(0, ncol = num.class, nrow = length(label))
  colnames(M) <- c(1:num.class)
  rownames(M) <- rev(label)

  for(i in 1:num.class) {
    sci <- single.class.importance(model_fn = model, feature_names_fn = names, i)
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

#' makes sci plot
#'
#' @param M importance matrix of model
#' @param savename
#' @param pdim plot dimension (height)
#' @param width plot width (pdim*width)
#' @import ggplot2
#' @import data.table

plt.sci <- function(M,
                    savename,
                    pdim,
                    width)  {


  cnames <- colnames(M)
  rnames <- rownames(M)
  #plot
  colnames(M) <- 1:ncol(M)
  rownames(M) <- 1:nrow(M)
  clr_palette <- "Greys"
  # plot rows along y, columns along x
  p <- ggplot(reshape2::melt(M)) +
    geom_raster(aes(y=Var1, x=Var2, fill=value)) +
    # coord_fixed() +
    theme_bw(base_size = pdim*2) +
    labs(title = "Single Class Importance",
         x="State",
         y="Feature") +
    theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = pdim*3),
          plot.subtitle = element_text(size = pdim),
          axis.title.x = element_text(face = "bold"),
          axis.text.x = element_text(size = pdim*1.2, angle = 360, vjust = 0.7),
          axis.title.y = element_text(face = "bold"),
          axis.text.y = element_text(),
          legend.title = element_blank(),
          legend.text = element_text(size = pdim*3)
    ) +
    scale_fill_distiller(palette=clr_palette,
                         direction = 1) +
    guides(fill = guide_colorbar(barheight = pdim, barwidth = pdim/10)) +
    scale_x_continuous(breaks=1:ncol(M),
                       labels = cnames,
                       expand=c(0,0)) +
    scale_y_reverse(breaks=1:nrow(M),
                    labels = rnames,
                    expand = c(0,0))

  ggsave(paste(savename, ".png", sep = ""), p, width = pdim*width, height = pdim)

}

