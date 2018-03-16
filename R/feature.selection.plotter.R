#' Accuracy loss plot based on feature selection
#'
#' Plot the accuracy as a function of the number of dismissed features.
#'
#' @param dir path of 'feature.selection' file (see \link{\code{feature.selection}})
#' @param pdim dimension of plot, default=10
#' @param width proportion of width to height, default=1.5
#' @param saveplot save plot?(T/F) default = T
#' @param pre plot prefix
#' @param xlim zoom on x-axis, e.g. xlim = 280:300
#' @param ylim zoom an y-axis, e.g. ylim = c(0,0.25). Use c() with other than natural numbers
#' @import data.table
#' @import ggplot2
#' @import RColorBrewer
#' @export

plt.feature.selection <- function(dir = "featureSelection",
                                  decreasing = F,
                                  pdim = 10,
                                  width = 1.5,
                                  saveplot = T,
                                  pre = "",
                                  xlim = NA,
                                  ylim = NA)  {
  #setwd(savefolder)
  # if(decreasing){
  #   dir <- "./feature.selection/decreasing"
  # } else {
  #   dir <- "./feature.selection/increasing"
  # }
  if(!dir.exists(paste(dir, "plot", sep = "/"))){
    dir.create(paste(dir, "plot", sep = "/"), showWarnings = F)
  }
  if(!file.exists(paste(dir, "/feature.selection", sep = "/")))  {
    stop("File 'feature.selection' not found. Run function 'feature.selection' first!")
  }
  M <- as.data.frame(fread(paste(dir, "feature.selection", sep = "/")))
  num.class <- dim(M)[2] - 3
  M <- M[,-2]
  colnames(M)[3:(num.class+2)] <- paste("State", 1:num.class)
  colnames(M)[2] <- "Accuracy"
  color <- c(1, colorRampPalette(brewer.pal(12, "Dark"))(num.class))#before: Paired

  p <- ggplot(melt(M, id.vars = "round")) +
    geom_point(aes(round, value, color = variable), size = 3, shape = 4) +
    geom_line(aes(round, value, color = variable), size = 2) +
    scale_size_manual(values = c(0,rep(pdim/5, num.class))) +
    scale_color_manual(values = color) +
    theme_bw(base_size = pdim*3) +
    labs(title = "Accuracy loss plot",
         x="number of dismissed features",
         y="accuracy") +
    theme(plot.title = element_text(face = "bold.italic", hjust = 0.5),
          plot.subtitle = element_text(size = pdim*2),
          axis.title.x = element_text(face = "bold"),
          axis.text.x = element_text(),
          axis.title.y = element_text(face = "bold"),
          axis.text.y = element_text(),
          legend.key.size = unit( 0.08, "npc"),
          legend.title = element_blank(),
          legend.text = element_text(face = "bold"),
          #legend.justification = c(1, 1),
          #legend.position = c(1, 1),
          legend.background = element_rect(colour = "black")
          )
  if(saveplot)  {
    ggsave(filename = paste(dir, "/plot/", pre, "all.png", sep = ""),
           p, width = pdim*width, height = pdim)
  }
  if(!is.na(xlim[1]) & !is.na(ylim[1]))  {
    p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
  } else if (!is.na(xlim[1]))  {
    p <- p + coord_cartesian(xlim = xlim)
  } else if (!is.na(ylim[1]))  {
    p <- p + coord_cartesian(ylim = ylim)
  }
  if(!is.na(xlim[1]) | !is.na(ylim[1]) & saveplot) {
    if(saveplot){
      ggsave(filename = paste(dir, "/plot/", pre, "zoom.png", sep = ""),
             p, width = pdim*width, height = pdim)
    }
  }
  if(!saveplot){
    return(p)
  }
}
