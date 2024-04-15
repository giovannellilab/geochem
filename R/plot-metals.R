library(ggplot2)
library(gridExtra)

plot_metals = function(df, log_y=FALSE) {

    figure_metals_abs = ggplot(
            data=df,
            aes(fill=element, y=concentration, x=sample)
        ) +
        geom_col(stat="identity") +
        xlab("Sample") +
        ylab("Concentration (ppb)") +
        scale_colour_manual(
            name="Metals",
            values=selected_metals,
            aesthetics="fill"
        ) +
        theme_glab() +
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

    figure_metals_rel = ggplot(
            data=df,
            aes(fill=element, y=concentration, x=sample)
        ) +
        geom_bar(position="fill", stat="identity") +
        xlab("Sample") +
        ylab("Concentration (%)") +
        scale_colour_manual(
            name="Metals",
            values=selected_metals,
            aesthetics="fill"
        ) +
        theme_glab() +
        theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

    if (log_y) {
        figure_metals_abs = figure_metals_abs +
            ylab("log(Concentration (ppb))") +
            scale_y_continuous(trans="log10")
    }

    plot = grid.arrange(
        figure_metals_abs,
        figure_metals_rel,
        nrow=2
    )

    return(plot)
}
