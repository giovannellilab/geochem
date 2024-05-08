#' Plots the absolute and relative metal concentration as bar plots
#' 
#' @param df Data frame containing sample, concentration and element columns
#' 
#' @return A `ggplot2` object.
#' 
#' @examples
#' plot_metals(df)
#' 
#' @seealso [geochem::process_icp()]
#' 
#' @import ggplot2
#' @import gridExtra
#' 
#' @export
plot_metals = function(df) {

    # Add absolute bar plot on the top
    figure_metals_abs = ggplot(
            data=df,
            aes(
                x=sample,
                y=1 + concentration,
                fill=element
            )
        ) +
        geom_col(stat="identity") +
        xlab("Sample") +
        ylab("log(1 + Concentration (ppb))") +
        scale_y_continuous(trans="log10") +

        # Manually color the metals
        scale_colour_manual(
            name="Metals",
            values=selected_metals,
            aesthetics="fill"
        ) +

        theme_glab() +
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

    # Add relative bar plot on the bottom
    figure_metals_rel = ggplot(
            data=df,
            aes(
                x=sample,
                y=concentration,
                fill=element
            )
        ) +
        geom_bar(position="fill", stat="identity") +
        xlab("Sample") +
        ylab("Concentration (%)") +

        # Manually color the metals
        scale_colour_manual(
            name="Metals",
            values=selected_metals,
            aesthetics="fill"
        ) +

        theme_glab() +
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

    plot = grid.arrange(
        figure_metals_abs,
        figure_metals_rel,
        nrow=2
    )

    return(plot)
}
