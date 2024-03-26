library(ggplot2)
library(ggtern)

plot_base_gigg_anions = function() {

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
        y=c(0.9, 0.55, 0.6, 1.0),
        z=c(0.0, 0.35, 0.4, 0.0)
    )

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
            color="black",
            alpha=0.5,
            linewidth=0.25,
            linetype="dashed"
        ) +

        # Add mature waters area
        geom_polygon(
            data=polygon,
            aes(
                x=x,
                y=y,
                z=z
            ),
            color="black",
            alpha=0.25,
            linewidth=0.15,
            linetype="dashed"
        ) +

        # Create the guides and labels
        guides(fill=guide_legend(override.aes=list(shape=21))) +
        labs(
            x=expression(SO[4]^-2),
            y=expression(Cl^-1),
            z=expression(HCO[3]^-1)
        ) +

        # Add annotations
        annotate(
            "text",
            label="Steam-heated waters",
            x=0.8,
            y=0.1,
            z=0.8,
            size=3,
            alpha=1.0,
            fontface="bold"
        ) +
        annotate(
            "text",
            label="Volcanic waters",
            x=0.7,
            y=0.3,
            z=0.05,
            size=3,
            alpha=1.0,
            angle=60,
            fontface="bold"
        ) +
        annotate(
            "text",
            label="Peripherial waters",
            x=0.05,
            y=0.3,
            z=0.7,
            size=3,
            alpha=1.0,
            angle=300,
            fontface="bold"
        ) +
        annotate(
            "text",
            label="Mature waters",
            x=0.2,
            y=1.75,
            z=0.7,
            size=3,
            alpha=1.0,
            angle=300,
            fontface="bold"
        ) +

        # Modify theme
        theme_hidemask() +
        theme_bw(base_size=15) +
        theme_hidegrid_major() +
        theme_nogrid_minor() +
        theme_nomask()

    return(plot)
}
