#' Plots the base ternary diagram for major anions (Giggenbach)
#' 
#' @return A `ggplot2` object.
#' 
#' @examples
#' plot_base_major_anions()
#' 
#' @seealso [geochem::plot_base_gibbs()]
#' @seealso [geochem::plot_base_ll()]
#' @seealso [geochem::plot_base_major_cations()]
#' @seealso [geochem::plot_base_piper()]
#' @seealso [geochem::plot_base_water_maturity()]
#' 
#' @importFrom ggplot2 aes_string geom_segment geom_polygon guides labs guide_legend
#' @importFrom ggtern annotate theme_showgrid theme_ticksoutside theme_ticklength_major
#' 
#' @export
#' 
plot_base_major_anions = function() {

  # Add division lines to the ternary plot
  lines = data.frame(
    x=c(0.5, 0.0, 0.5),
    y=c(0.5, 0.5, 0.0),
    z=c(0.0, 0.5, 0.5),
    xend=c(1, 1, 1) / 3,
    yend=c(1, 1, 1) / 3,
    zend=c(1, 1, 1) / 3
  )

  # Add mature waters area
  polygon = data.frame(
    x=c(0.1, 0.1, 0.0, 0.0),
    y=c(0.9, 0.6, 0.6, 1.0),
    z=c(0.0, 0.3, 0.4, 0.0)
  )

  plot = ggtern::ggtern() +

    # Add division lines
    ggplot2::geom_segment(
      data=lines,
      ggplot2::aes_string(
        x="x",
        y="y",
        z="z",
        xend="xend",
        yend="yend",
        zend="zend"
      ),
      color="grey",
      alpha=0.5,
      linewidth=0.25,
      linetype="dashed"
    ) +

    # Add mature waters area
    ggplot2::geom_polygon(
      data=polygon,
      ggplot2::aes_string(
        x="x",
        y="y",
        z="z"
      ),
      fill="grey",
      alpha=0.15,
      color="grey",
      linewidth=0.25,
      linetype="dashed"
    ) +

    # Create the guides and labels
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(shape=21))) +
    ggplot2::labs(
      x=expression(SO[4]^-2),
      y=expression(Cl^-1),
      z=expression(HCO[3]^-1)
    ) +

    # Add annotations
    ggtern::annotate(
      "text",
      label="Steam heated waters",
      color="grey",
      x=0.7,
      y=0.05,
      z=0.3,
      size=3,
      alpha=1.0,
      fontface="bold"
    ) +
    ggtern::annotate(
      "text",
      label="Sulfate waters",
      color="grey",
      x=0.75,
      y=0.25,
      z=0.05,
      size=3,
      alpha=1.0,
      angle=60,
      fontface="bold"
    ) +
    ggtern::annotate(
      "text",
      label="Bicarbonate waters",
      color="grey",
      x=0.05,
      y=0.3,
      z=0.7,
      size=3,
      alpha=1.0,
      angle=300,
      fontface="bold"
    ) +
    ggtern::annotate(
      "text",
      label="Chloride waters",
      color="grey",
      x=0.2,
      y=2.25,
      z=0.7,
      size=3,
      alpha=1.0,
      angle=300,
      fontface="bold"
    ) +

    # Add theme options
    ggtern::theme_bw() +
    ggtern::theme_showgrid() +
    ggtern::theme_ticksoutside() +
    ggtern::theme_ticklength_major(major=unit(3.5, "mm"))

  return(plot)
}
