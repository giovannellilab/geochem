library(ggplot2)
library(gridExtra)

plot_metals = function(df) {

    # Add absolute barplot on the top
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

    plot = grid.arrange(
        figure_metals_abs,
        figure_metals_rel,
        nrow=2
    )

    return(plot)
}
