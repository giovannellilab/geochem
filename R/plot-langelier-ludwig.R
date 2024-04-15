library(tidyverse)

plot_base_ll = function() {

    plot = ggplot() +

        # Format axes
        scale_x_continuous(
            name=expression("R"("HCO"[3]^"-")),
            limits=c(0, 50),
            breaks=seq(from=0, to=50, by=10),
            sec.axis=sec_axis(
                r_cl_so4~.,
                name=expression("R"("Cl"^"-" + "SO"[4]^"2-")),
                labels=rev(seq(from=0, to=50, by=10))
            )
        ) +
        scale_y_continuous(
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
        geom_rect(
            aes(xmin=0, xmax=7, ymin=45, ymax=50),
            fill="cadetblue 1",
            color="black",
            linewidth=0.2,
            alpha=0.5
        ) +
        annotate(
            geom="text",
            x=3.5,
            y=47.5,
            label="Halite dissolution",
            color="black",
            size=2.15,
            alpha=0.5
        ) +

        geom_rect(
            aes(xmin=12.5, xmax=23, ymin=44, ymax=49),
            fill="coral 1",
            color="black",
            linewidth=0.2,
            alpha=0.3
        ) +
        annotate(
            geom="text",
            x=17.65,
            y=46.5,
            label="Geothermal brines",
            color="black",
            size=2.75,
            alpha=0.5
        ) +

        geom_rect(
            aes(xmin=44, xmax=50, ymin=39, ymax=50),
            fill="darkolivegreen 1",
            color="black",
            linewidth=0.2,
            alpha=0.3
        ) +
        annotate(
            geom="text",
            x=47,
            y=44.5,
            label="Na-HCO3 waters",
            color="black",
            size=2.25,
            alpha=0.5,
            angle=270
        ) +

        geom_rect(
            aes(xmin=0, xmax=7, ymin=37, ymax=45),
            fill="cadetblue 3",
            color="black",
            linewidth=0.2,
            alpha=0.9
        ) +
        annotate(
            geom="text",
            x=3.5,
            y=41.0,
            label="Marine waters",
            color="black",
            size=2.5,
            alpha=0.5
        ) +

        geom_rect(
            aes(xmin=0, xmax=2.5, ymin=0, ymax=20),
            fill="darkolivegreen 3",
            color="black",
            linewidth=0.2,
            alpha=0.9
        ) +
        annotate(
            geom="text",
            x=1.25,
            y=10.0,
            label="Acidic waters / Interactions with H2S",
            color="black",
            size=2.5,
            alpha=0.5,
            angle=90
        ) +

        geom_rect(
            aes(xmin=10, xmax=20, ymin=2.5, ymax=10),
            fill="cornsilk 1",
            color="black",
            linewidth=0.2,
            alpha=0.9
        ) +
        annotate(
            geom="text",
            x=15.0,
            y=6.0,
            label="Gypsum dissolution /\n Interaction with sulfate \n minerals",
            color="black",
            size=2.25,
            alpha=0.5
        ) +

        geom_rect(
            aes(xmin=37, xmax=50, ymin=0, ymax=15),
            fill="aliceblue",
            color="black",
            linewidth=0.2,
            alpha=0.9
        ) +
        annotate(
            geom="text",
            x=43.5,
            y=7.5,
            label="Groundwaters",
            color="black",
            size=2.5,
            alpha=0.5
        ) +

        # Add lines
        geom_vline(
            xintercept=25,
            linetype="dashed",
            color="gray",
            linewidth=0.2
        ) +
        geom_hline(
            yintercept=25,
            linetype="solid",
            color="gray",
            linewidth=0.2
        ) +

        # Add arrows
        geom_segment(
            aes(x=36.5, y=14, xend=7.5, yend=37.5),
            lineend="butt",
            linejoin="mitre",
            linewidth=0.5,
            arrow=arrow(length=unit(0.15, "inches")),
            colour="grey"
        ) +
        annotate(
            geom="text",
            x=20.5,
            y=23.5,
            label="Mixing line",
            color="darkgrey",
            size=3.5,
            alpha=0.5
        ) +
        geom_segment(
            aes(x=7.5, y=44.5, xend=37.5, yend=44.5),
            lineend="butt",
            linejoin="mitre",
            linewidth=0.5,
            arrow=arrow(length=unit(0.15, "inches")),
            colour="grey"
        ) +
        annotate(
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
        theme(aspect.ratio=1)

    return(plot)
}
