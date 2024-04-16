library(ggplot2)

plot_base_gibbs = function() {
    plot = ggplot() +

        # Add lines
        geom_segment(
            aes(
                x=0.05,
                y=1e+2,
                xend=0.9,
                yend=10
            ),
            colour="lightgrey",
            alpha=0.3
        ) +
        geom_segment(
            aes(
                x=0.05,
                y=1e+3,
                xend=0.9,
                yend=1e+5
            ),
            colour="lightgrey",
            alpha=0.3
        ) +
        geom_segment(
            aes(
                x=0.6,
                y=1e+3,
                xend=0.9,
                yend=1e+2
            ),
            colour="lightgrey",
            alpha=0.3
        ) +
        geom_segment(
            aes(
                x=0.6,
                y=1e+3,
                xend=0.9,
                yend=1e+4
            ),
            colour="lightgrey",
            alpha=0.3
        ) +

        # Add annotations
        annotate(
            geom="text",
            x=0.2,
            y=2e+2,
            label="Rock",
            color="grey",
            size=3,
            alpha=0.6
        ) +
        annotate(
            geom="text",
            x=0.6,
            y=1e+2,
            label="Precipitation",
            color="grey",
            size=3,
            alpha=0.6
        ) +
        annotate(
            geom="text",
            x=0.4,
            y=2e+4,
            label="Evaporation",
            color="grey",
            size=3,
            alpha=0.6
        ) +

        # Format axes
        scale_y_log10() +
        annotation_logticks(sides="l") +

        labs(
            x="Na/(Na+Ca)",
            y="TDS (ppm)",
            fill="Temperature (°C)"
        ) +

        theme_glab()

    return(plot)
}
