library(ggplot2)
library(ggtern)

plot_base_gigg_anions = function() {

    # Add lines to the ternary plot
    lines = data.frame(
        x=c(0.5, 0.0, 0.5),
        y=c(0.5, 0.5, 0.0),
        z=c(0.0, 0.5, 0.5),
        xend=c(1, 1, 1) / 3,
        yend=c(1, 1, 1) / 3,
        zend=c(1, 1, 1) / 3
    ) 

    plot = ggtern() +

        # Add division
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
            linewidth=0.25,
            linetype="dashed"
        ) +

        # Create the guides and labels
        guides(fill=guide_legend(override.aes=list(shape=21))) +
        labs(
            x=expression(SO[4]^-2),
            y=expression(Cl^-1),
            z=expression(HCO[3]^-1)
        ) +

        # Modify theme
        theme_hidemask() +
        theme_bw(base_size=15) +
        theme_hidegrid_major() +
        theme_nogrid_minor() +
        theme_nomask()

    return(plot)
}
