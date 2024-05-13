#' Plots the base Gibbs diagram
#' 
#' @return A `ggplot2` object.
#' 
#' @examples
#' plot_base_gibbs()
#' 
#' @seealso [geochem::plot_base_major_anions()]
#' @seealso [geochem::plot_base_ll()]
#' @seealso [geochem::plot_base_major_cations()]
#' @seealso [geochem::plot_base_piper()]
#' @seealso [geochem::plot_base_water_maturity()]
#' 
#' @importFrom ggplot2 ggplot geom_segment aes scale_y_log10 annotation_logticks labs
#' 
#' @export
plot_base_gibbs = function() {
  plot = ggplot2::ggplot() +

    # Add lines
    ggplot2::geom_segment(
      ggplot2::aes(
        x=0.05,
        y=1e+2,
        xend=0.9,
        yend=10
      ),
      colour="lightgrey",
      alpha=0.3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x=0.05,
        y=1e+3,
        xend=0.9,
        yend=1e+5
      ),
      colour="lightgrey",
      alpha=0.3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x=0.6,
        y=1e+3,
        xend=0.9,
        yend=1e+2
      ),
      colour="lightgrey",
      alpha=0.3
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x=0.6,
        y=1e+3,
        xend=0.9,
        yend=1e+4
      ),
      colour="lightgrey",
      alpha=0.3
    ) +

    # Add annotations
    ggplot2::annotate(
      geom="text",
      x=0.2,
      y=2e+2,
      label="Rock",
      color="grey",
      size=3,
      alpha=0.6
    ) +
    ggplot2::annotate(
      geom="text",
      x=0.6,
      y=1e+2,
      label="Precipitation",
      color="grey",
      size=3,
      alpha=0.6
    ) +
    ggplot2::annotate(
      geom="text",
      x=0.4,
      y=2e+4,
      label="Evaporation",
      color="grey",
      size=3,
      alpha=0.6
    ) +

    # Format axes
    ggplot2::scale_y_log10() +
    ggplot2::annotation_logticks(sides="l") +

    ggplot2::labs(
      x="Na / (Na+Ca)",
      y="TDS (ppm)",
      fill="Temperature (°C)"
    ) +

    theme_glab()

  return(plot)
}
