#' Plots the absolute and relative metal concentration as bar plots
#' 
#' @param df Data frame containing sample, concentration and element columns
#' 
#' @return A `ggplot2` object.
#' 
#' @seealso [geochem::process_icp()]
#' 
#' @importFrom ggplot2 ggplot aes geom_col xlab ylab scale_y_continuous scale_color_manual theme
#' @importFrom gridExtra grid.arrange
#' @importFrom tibble deframe
#' @import checkmate
#' 
#' @export
#' 
plot_metals = function(df) {

  checkmate::assertDataFrame(
    x=df,
    col.names="named",
    ncols=5
  )
  checkmate::assertSetEqual(
    x=colnames(df),
    y=c("sample", "element", "concentration", "type", "color")
  )

  color_map = df %>%
    ungroup() %>%
    select("element", "color") %>%
    unique() %>%
    tibble::deframe()

  # Add absolute bar plot on the top
  figure_metals_abs = ggplot2::ggplot(
      data=df,
      ggplot2::aes(
        x=get("sample"),
        y=1 + get("concentration"),
        fill=get("element")
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::xlab("Sample") +
    ggplot2::ylab("log(1 + Concentration (ppb))") +
    ggplot2::scale_y_continuous(trans="log10") +
    
    # Add facet_grid
    ggplot2::facet_grid(cols=vars(get("type"))) +
    
    # Manually color the metals
    ggplot2::scale_colour_manual(
      name="Metals",
      values=color_map,
      aesthetics="fill"
    ) +

    theme_glab() +
    ggplot2::theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

  # Add relative bar plot on the bottom
  figure_metals_rel = ggplot2::ggplot(
      data=df,
      ggplot2::aes(
        x=get("sample"),
        y=get("concentration"),
        fill=get("element")
      )
    ) +
    ggplot2::geom_bar(position="fill", stat="identity") +
    ggplot2::xlab("Sample") +
    ggplot2::ylab("Concentration (%)") +
    
    # Add facet_grid
    facet_grid(cols=vars(get("type"))) +
    
    # Manually color the metals
    ggplot2::scale_colour_manual(
      name="Metals",
      values=color_map,
      aesthetics="fill"
    ) +

    theme_glab() +
    ggplot2::theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

  plot = gridExtra::grid.arrange(
    figure_metals_abs,
    figure_metals_rel,
    nrow=2
  )

  return(plot)
}
