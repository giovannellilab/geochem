#' Giovannelli lab theme (https://github.com/giovannellilab/GLab_ggplot2_theme)
#' 
#' @param base_size Default text size
#' @param base_family Default text font
#' @param base_line_size Default line size
#' @param base_rect_size Default rect size (not used)
#' 
#' @return The Giovannelli lab theme.
#' 
#' @import ggplot2
#' 
theme_glab = function(
  base_size=12,
  base_family="",
  base_line_size=base_size / 180,
  base_rect_size=base_size / 180
) {

  # Assign font family up front
  font = "Helvetica"

  ggplot2::theme_bw(
    base_size=base_size, 
    base_family=base_family,
    base_line_size=base_line_size
  ) %+replace%
  ggplot2::theme(
    legend.background=element_blank(),
    legend.title=element_text(
      color=rgb(100, 100, 100, maxColorValue=255),
      size=rel(0.65),
      hjust=0
    ),
    legend.text=element_text(
      color=rgb(100, 100, 100, maxColorValue=255),
      size=rel(0.65)
    ),
    legend.key.size=unit(0.8, "lines"),
    plot.title=element_text(
      color=rgb(100, 100, 100, maxColorValue=255), 
      hjust=0
    ),
    axis.title=element_text(
      color=rgb(100, 100, 100, maxColorValue=255),
      size=rel(0.65)
    ),
    axis.text=element_text(
      color=rgb(100, 100, 100, maxColorValue=255),
      size=rel(0.65)
    ),
    plot.caption=element_text(
      color=rgb(100, 100, 100, maxColorValue=255),
      size=rel(0.35),
      hjust=1
    ), 
    panel.grid.major=element_blank(),   
    panel.grid.minor=element_blank(),   
    panel.border=element_rect(
      colour=rgb(100, 100, 100, maxColorValue=255),
      fill=NA
    ),
    complete=TRUE
  )
}
