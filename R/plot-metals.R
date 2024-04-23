library(ggplot2)
library(gridExtra)

plot_metals = function(df, log_y=FALSE) {

    # Add absolute barplot on the top
    figure_metals_abs = ggplot(
            data=df,
            aes(
                x=sample,
                y=concentration,
                fill=element
            )
        ) +
        geom_col(stat="identity") +
        xlab("Sample") +
        ylab("Concentration (ppb)") +

        # Manually color the metals
        scale_colour_manual(
            name="Metals",
            values=selected_metals,
            aesthetics="fill"
        ) +

        theme_glab() +
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

    # Add relative barplot on the bottom
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

    # Scale y axis in the absolute barplot
    if (log_y) {
        figure_metals_abs = figure_metals_abs +
            ylab("log(Concentration (ppb))") +
            scale_y_continuous(trans="log10") +

            # Add horizontal line on y=0
            geom_hline(
                yintercept=1,
                linetype="solid",
                color="grey"
            )
    }

    plot = grid.arrange(
        figure_metals_abs,
        figure_metals_rel,
        nrow=2
    )

    return(plot)
}
