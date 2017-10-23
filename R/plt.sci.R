#' single class importance plotter
#' 
#' plots importance from single.class.importance
#'
#' @param M importance matrix of model
#' @param colmin minimum, each feature has to contribute to at least one class
#' @param pdim plot dimension (height)
#' @param width plot width (pdim*width)
#' @import ggplot2
#' @import data.table
#' @export

plt.sci <- function(M,
                    colmin,
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
  
  ggsave(paste("single.class.importance/sci_colmin", colmin, ".png", sep = ""), p, width = pdim*width, height = pdim)
  
}
