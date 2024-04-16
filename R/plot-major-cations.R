library(ggplot2)
library(ggtern)

plot_base_major_cations = function() {

    # Add division lines to the ternary plot
    lines = data.frame(
        x=c(0.5, 0.0, 0.5),
        y=c(0.5, 0.5, 0.0),
        z=c(0.0, 0.5, 0.5),
        xend=c(1, 1, 1) / 3,
        yend=c(1, 1, 1) / 3,
        zend=c(1, 1, 1) / 3
    )

    # Create plot
    plot = ggtern() +

        # Add division lines
        geom_segment(
            data=lines,
            aes(
                x=x,
                y=y,
                z=z,
                xend=xend,
                yend=yend,
                zend=zend
            ),
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
            linetype="dashed"
        ) +

        # Create the guides and labels
        guides(fill=guide_legend(override.aes=list(shape=21))) +
        labs(
            x="Ca",
            y="Mg",
            z="Na + K",
            fill="samples"
        ) +

        # Add theme options
        ggtern::theme_showgrid() +
        ggtern::theme_ticksoutside() +
        ggtern::theme_ticklength_major(major=unit(3.5, "mm"))

    return(plot)
}
