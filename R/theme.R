#' Giovannelli lab theme (https://github.com/giovannellilab/GLab_ggplot2_theme)
#' 
#' @param base_size Default text size
#' @param base_family Default text font
#' @param base_line_size Default line size
#' @param base_rect_size Default rect size (not used)
#' 
#' @return The Giovannelli lab theme.
#' 
#' @importFrom ggplot2 theme_bw theme '%+replace%' element_blank element_text element_rect rel unit
#' @importFrom grDevices rgb
#' 
#' @export
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
    legend.background=ggplot2::element_blank(),
    legend.title=ggplot2::element_text(
      color=grDevices::rgb(100, 100, 100, maxColorValue=255),
      size=ggplot2::rel(0.65),
      hjust=0
    ),
    legend.text=ggplot2::element_text(
      color=grDevices::rgb(100, 100, 100, maxColorValue=255),
      size=ggplot2::rel(0.65)
    ),
    legend.key.size=ggplot2::unit(0.8, "lines"),
    plot.title=ggplot2::element_text(
      color=grDevices::rgb(100, 100, 100, maxColorValue=255), 
      hjust=0
    ),
    axis.title=ggplot2::element_text(
      color=grDevices::rgb(100, 100, 100, maxColorValue=255),
      size=ggplot2::rel(0.65)
    ),
    axis.text=ggplot2::element_text(
      color=grDevices::rgb(100, 100, 100, maxColorValue=255),
      size=ggplot2::rel(0.65)
    ),
    plot.caption=ggplot2::element_text(
      color=grDevices::rgb(100, 100, 100, maxColorValue=255),
      size=ggplot2::rel(0.35),
      hjust=1
    ), 
    panel.grid.major=ggplot2::element_blank(),   
    panel.grid.minor=ggplot2::element_blank(),   
    panel.border=ggplot2::element_rect(
      colour=grDevices::rgb(100, 100, 100, maxColorValue=255),
      fill=NA
    ),
    complete=TRUE
  )
}
