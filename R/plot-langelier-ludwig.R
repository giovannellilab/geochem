#' Plots the base Langelier-Ludwig diagram
#' 
#' @return A `ggplot2` object.
#' 
#' @examples
#' plot_base_ll()
#' 
#' @seealso [geochem::plot_base_gibbs()]
#' @seealso [geochem::plot_base_major_anions()]
#' @seealso [geochem::plot_base_major_cations()]
#' @seealso [geochem::plot_base_piper()]
#' @seealso [geochem::plot_base_water_maturity()]
#' @seealso [geochem::process_ic()]
#' 
#' @references{
#'   \insertRef{langelier1942_GraphicalMethodsIndicating}{geochem}
#' }
#' 
#' @importFrom ggplot2 ggplot scale_x_continuous scale_y_continuous geom_rect aes  geom_vline geom_hline theme
#' 
#' @export
plot_base_ll = function() {

  plot = ggplot2::ggplot() +

    # Format axes
    ggplot2::scale_x_continuous(
      name=expression("R"("HCO"[3]^"-")),
      limits=c(0, 50),
      breaks=seq(from=0, to=50, by=10),
      sec.axis=sec_axis(
        r_cl_so4~.,
        name=expression("R"("Cl"^"-" + "SO"[4]^"2-")),
        labels=rev(seq(from=0, to=50, by=10))
      )
    ) +
    ggplot2::scale_y_continuous(
      name=expression("R"("Na"^"+" + "K"^"+")),
      limits=c(0, 50),
      breaks=seq(from=0, to=50, by=10),
      sec.axis=sec_axis(
        r_ca_mg~.,
        name=expression("R"("Ca"^"2+" + "Mg"^"2+")),
        labels=rev(seq(from=0, to=50, by=10))
      )
    ) +

    # Add annotations
    ggplot2::geom_rect(
      ggplot2::aes(xmin=0, xmax=7, ymin=45, ymax=50),
      fill="cadetblue 1",
      color="black",
      linewidth=0.25,
      alpha=0.5
    ) +
    ggplot2::annotate(
      geom="text",
      x=3.5,
      y=47.5,
      label="Halite dissolution",
      color="black",
      size=2.15,
      alpha=0.5
    ) +

    ggplot2::geom_rect(
      ggplot2::aes(xmin=12.5, xmax=23, ymin=44, ymax=49),
      fill="coral 1",
      color="black",
      alpha=0.3,
      linewidth=0.25
    ) +
    ggplot2::annotate(
      geom="text",
      x=17.65,
      y=46.5,
      label="Geothermal brines",
      color="black",
      size=2.75,
      alpha=0.5
    ) +

    ggplot2::geom_rect(
      ggplot2::aes(xmin=44, xmax=50, ymin=39, ymax=50),
      fill="darkolivegreen 1",
      color="black",
      alpha=0.3,
      linewidth=0.25
    ) +
    ggplot2::annotate(
      geom="text",
      x=47,
      y=44.5,
      label="Na-HCO3 waters",
      color="black",
      size=2.25,
      alpha=0.5,
      angle=270
    ) +

    ggplot2::geom_rect(
      ggplot2::aes(xmin=0, xmax=7, ymin=37, ymax=45),
      fill="cadetblue 3",
      color="black",
      alpha=0.9,
      linewidth=0.25
    ) +
    ggplot2::annotate(
      geom="text",
      x=3.5,
      y=41.0,
      label="Marine waters",
      color="black",
      size=2.5,
      alpha=0.5
    ) +

    ggplot2::geom_rect(
      ggplot2::aes(xmin=0, xmax=2.5, ymin=0, ymax=20),
      fill="darkolivegreen 3",
      color="black",
      alpha=0.9,
      linewidth=0.25
    ) +
    ggplot2::annotate(
      geom="text",
      x=1.25,
      y=10.0,
      label="Acidic waters / Interactions with H2S",
      color="black",
      size=2.5,
      alpha=0.5,
      angle=90
    ) +

    ggplot2::geom_rect(
      ggplot2::aes(xmin=10, xmax=20, ymin=2.5, ymax=10),
      fill="cornsilk 1",
      color="black",
      alpha=0.9,
      linewidth=0.25
    ) +
    ggplot2::annotate(
      geom="text",
      x=15.0,
      y=6.0,
      label="Gypsum dissolution /\n Interaction with sulfate \n minerals",
      color="black",
      size=2.25,
      alpha=0.5
    ) +

    ggplot2::geom_rect(
      ggplot2::aes(xmin=37, xmax=50, ymin=0, ymax=15),
      fill="aliceblue",
      color="black",
      alpha=0.9,
      linewidth=0.25
    ) +
    ggplot2::annotate(
      geom="text",
      x=43.5,
      y=7.5,
      label="Groundwaters",
      color="black",
      size=2.5,
      alpha=0.5
    ) +

    # Add lines
    ggplot2::geom_vline(
      xintercept=25,
      linetype="dashed",
      color="gray",
      linewidth=0.25
    ) +
    ggplot2::geom_hline(
      yintercept=25,
      linetype="solid",
      color="gray",
      linewidth=0.25
    ) +

    # Add arrows
    ggplot2::geom_segment(
      ggplot2::aes(x=36.5, y=14, xend=7.5, yend=37.5),
      lineend="butt",
      linejoin="mitre",
      linewidth=0.5,
      arrow=arrow(length=unit(0.15, "inches")),
      colour="grey"
    ) +
    ggplot2::annotate(
      geom="text",
      x=20.5,
      y=23.5,
      label="Mixing line",
      color="darkgrey",
      size=3.5,
      alpha=0.5
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x=7.5, y=44.5, xend=37.5, yend=44.5),
      lineend="butt",
      linejoin="mitre",
      linewidth=0.5,
      arrow=arrow(length=unit(0.15, "inches")),
      colour="grey"
    ) +
    ggplot2::annotate(
      geom="text",
      x=25,
      y=43,
      parse=TRUE,
      label="'Interaction with CO'[2]",
      color="darkgrey",
      size=3.5,
      alpha=0.5
    ) +

    theme_glab() +

    # Force the diagram to be squared
    ggplot2::theme(aspect.ratio=1)

  return(plot)
}
