# Adapted from the following sources:
# https://gist.github.com/johnDorian/5561272
# https://github.com/markolipka/ggplot_Piper/blob/master/ggplot_Piper.R

library(ggplot2)

transform_data_piper = function(Mg, Ca, Cl, SO4, id) {

    y1 = Mg * 0.86603
    x1 = 100 * (1 - (Ca/100) - (Mg/200))
    y2 = SO4 * 0.86603
    x2 = 120 + (100 * Cl/100 + 0.5 * 100 * SO4/100)

    new_point = function(x1, x2, y1, y2, grad=1.73206){
        b1 = y1 - (grad * x1)
        b2 = y2 - (-grad * x2)
        M = matrix(c(grad, -grad, -1, -1), ncol=2)
        intercepts = as.matrix(c(b1,b2))
        t_mat = -solve(M) %*% intercepts
        data.frame(x=t_mat[1,1], y=t_mat[2,1])
    }

    np_list = lapply(
        1:length(x1),
        function(i) new_point(x1[i], x2[i], y1[i], y2[i])
    )
    npoints = do.call("rbind", np_list)

    return(
        data.frame(
            id=id,
            x=c(x1, x2, npoints$x),
            y=c(y=y1, y2, npoints$y)
        )
    )
}

plot_base_piper = function() {

    # ------------------------------------------------------------------------ #
    # Create base plot

    plot = ggplot() +

        # -------------------------------------------------------------------- #
        # Add water regions (see https://inside.mines.edu/~epoeter/_GW/18WaterChem2/WaterChem2pdf.pdf)

        # Na-HCO3 waters (deeper ground waters influenced by ion exchange)
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(110, 85, 135),
                y=c(17.3206,  60.6221, 60.6221)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Mixing zone (lower)
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(110, 85, 135),
                y=c(103.9236,  60.6221, 60.6221)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Na-Cl waters (marine and deep ancient ground waters)
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(135, 110, 135, 160),
                y=c(60.6221, 103.9236, 147.2251, 103.9236)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Ca-HCO3 waters (shallow, fresh ground waters)
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(85, 60, 85, 110),
                y=c(60.6221, 103.9236, 147.2251, 103.9236)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Ca-SO4 waters (gypsum ground waters and mine drainage)
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(110, 85, 110, 135),
                y=c(147.2251, 147.2251, 190.5266, 147.2251)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Mixing zone (upper)
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(110, 85, 135),
                y=c(103.9236, 147.2251, 147.2251)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # -------------------------------------------------------------------- #
        # Add cations regions

        # Mg-Ca region
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(0, 25, 50),
                y=c(0, 43.3015, 0)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Mg-NA+K region
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(25, 50, 75),
                y=c(43.3015, 86.603, 43.3015)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Ca-NA+K region
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(50, 75, 100),
                y=c(0, 43.3015, 0)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # -------------------------------------------------------------------- #
        # Add anions regions

        # HCO3-Cl region
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(120, 145, 170),
                y=c(0, 43.3015, 0)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # HCO3-SO4 region
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(145, 170, 195),
                y=c(43.3015, 86.603, 43.3015)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # Cl-SO4 region
        geom_polygon(
            aes(
                x=x,
                y=y
            ),
            data=data.frame(
                x=c(170, 195, 220),
                y=c(0, 43.3015, 0)
            ),
            fill="white",
            color="grey",
            alpha=0.5,
            size=0.25, # linewidth in newer ggplot2 versions
        ) +

        # -------------------------------------------------------------------- #
        # Add shapes

        # Left ternary plot (cations)
        geom_segment(aes(x=0,  y=0,         xend=100, yend=0)) +
        geom_segment(aes(x=0,  y=0,         xend=50,  yend=86.603)) +
        geom_segment(aes(x=50, y=86.603,    xend=100, yend=0)) +

        # Right ternary plot (anions)
        geom_segment(aes(x=120, y=0,        xend=220, yend=0)) +
        geom_segment(aes(x=120, y=0,        xend=170, yend=86.603)) +
        geom_segment(aes(x=170, y=86.603,   xend=220, yend=0)) +

        # Upper diamond
        geom_segment(aes(x=110, y=190.5266, xend=60,  yend=103.9236)) +
        geom_segment(aes(x=110, y=190.5266, xend=160, yend=103.9236)) +
        geom_segment(aes(x=110, y=17.3206,  xend=160, yend=103.9236)) +
        geom_segment(aes(x=110, y=17.3206,  xend=60,  yend=103.9236)) +

        # -------------------------------------------------------------------- #
        # Add axes ticks

        # X Cation
        geom_text(
            aes(
                x=c(20, 40, 60, 80),
                y=c(-5, -5, -5, -5),
                label=c(80, 60, 40, 20)
            ),
            size=2.5
        ) +
        # Y Cation
        geom_text(
            aes(
                x=c(35, 25, 15, 5),
                y=c(69.2824, 51.9618, 34.6412, 17.3206),
                label=c(80, 60, 40, 20)
            ),
            size=2.5
        ) +
        # Z Cation
        geom_text(
            aes(
                x=c(95, 85, 75, 65),
                y=c(17.3206, 34.6412, 51.9618, 69.2824),
                label=c(80, 60, 40, 20)
            ),
            size=2.5
        ) +

        coord_equal(ratio=1) +

        # Y Anion 
        geom_text(
            aes(
                x=c(155, 145, 135, 125),
                y=c(69.2824, 51.9618, 34.6412, 17.3206),
                label=c(20, 40, 60, 80)
            ),
            size=2.5
        ) +
        # Z Anion
        geom_text(
            aes(
                x=c(215, 205, 195, 185),
                y=c(17.3206, 34.6412, 51.9618, 69.2824),
                label=c(20, 40, 60, 80)
            ),
            size=2.5
        ) +
        # X Anion
        geom_text(
            aes(
                x=c(140, 160, 180, 200),
                y=c(-5, -5, -5, -5),
                label=c(20, 40, 60, 80)
            ),
            size=2.5
        ) +

        # Left upper diamond
        geom_text(
            aes(
                x=c(65, 75, 85, 95),
                y=c(121.2442, 138.5648, 155.8854, 173.2060),
                label=c(20, 40, 60, 80)
            ),
            size=2.5
        ) +
        # Right upper diamond
        geom_text(
            aes(
                x=c(155, 145, 135, 125),
                y=c(121.2442, 138.5648, 155.8854, 173.2060),
                label=c(20, 40, 60, 80)
            ),
            size=2.5
        ) +

        # -------------------------------------------------------------------- #
        # Add axes labels
        geom_text(
            aes(
                x=17,
                y=50,
                label="Mg^'2+'"
            ),
            angle=60,
            size=3.5,
            parse=TRUE
        ) +
        geom_text(
            aes(
                x=82.5,
                y=51.5,
                label="Na^'+'~+~K^'+'"
            ),
            angle=-60,
            size=3.5,
            parse=TRUE
        ) +
        geom_text(
            aes(
                x=50,
                y=-10,
                label="Ca^'2+'"
            ),
            size=3.5,
            parse=TRUE
        ) +
        geom_text(
            aes(
                x=170,
                y=-10,
                label="Cl^'-'"
            ),
            size=3.5,
            parse=TRUE
        ) +
        geom_text(
            aes(
                x=205,
                y=50,
                label="SO[4]^'2-'"
            ),
            angle=-60,
            size=3.5,
            parse=TRUE
        ) +
        geom_text(
            aes(
                x=137.5,
                y=51.5,
                label="HCO[3]^'-'"
            ),
            angle=60,
            size=3.5,
            parse=TRUE
        ) +
        geom_text(
            aes(
                x=72.5,
                y=150,
                label="SO[4]^'2-'~+~Cl^'-'"
            ),
            angle=60,
            size=3.5,
            parse=TRUE
        ) +
        geom_text(
            aes(
                x=147.5,
                y=150,
                label="Ca^'2+'~+~Mg^'2+'"
            ),
            angle=-60,
            size=3.5,
            parse=TRUE
        ) + 

        # -------------------------------------------------------------------- #
        # Remove axes

        theme_bw() +
        theme(
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            axis.ticks=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()
        )

    return(plot)
}
