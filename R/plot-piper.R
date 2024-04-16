# Adapted from https://gist.github.com/johnDorian/5561272

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

    plot = ggplot() +

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
        # Add grid lines

        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(20, 40, 60, 80),
                x2=c(10, 20, 30, 40),
                y1=c(0, 0, 0, 0),
                y2=c(17.3206, 34.6412, 51.9618, 69.2824)
            ),
            size=0.25,
            colour="grey"
        ) +
        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(20, 40, 60, 80),
                x2=c(60, 70, 80, 90),
                y1=c(0, 0, 0, 0),
                y2=c(69.2824, 51.9618, 34.6412, 17.3206)
            ),
            size=0.25,
            colour="grey"
        ) +
        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(10, 20, 30, 40),
                x2=c(90, 80, 70, 60),
                y1=c(17.3206, 34.6412, 51.9618, 69.2824),
                y2=c(17.3206, 34.6412, 51.9618, 69.2824)
            ),
            size=0.25,
            colour="grey"
        ) +
        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(140, 160, 180, 200),
                x2=c(130, 140, 150, 160),
                y1=c(0, 0, 0, 0),
                y2=c(17.3206, 34.6412, 51.9618, 69.2824)
            ),
            size=0.25,
            colour="grey"
        ) +
        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(140, 160, 180, 200),
                x2=c(180, 190, 200, 210),
                y1=c(0, 0, 0, 0),
                y2=c(69.2824, 51.9618, 34.6412, 17.3206)
            ),
            size=0.25,
            colour="grey"
        ) +
        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(130, 140, 150, 160),
                x2=c(210, 200, 190, 180),
                y1=c(17.3206, 34.6412, 51.9618, 69.2824),
                y2=c(17.3206, 34.6412, 51.9618, 69.2824)
            ),
            size=0.25,
            colour="grey"
        ) +
        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(100, 90, 80, 70),
                y1=c(34.6412, 51.9618, 69.2824, 86.603),
                x2=c(150, 140, 130, 120),
                y2=c(121.2442, 138.5648, 155.8854, 173.2060)
            ),
            size=0.25,
            colour="grey"
        ) +
        geom_segment(
            aes(x=x1, y=y1, yend=y2, xend=x2),
            data=data.frame(
                x1=c(70, 80, 90, 100),
                y1=c(121.2442, 138.5648, 155.8854, 173.2060),
                x2=c(120, 130, 140, 150),
                y2=c(34.6412, 51.9618, 69.2824, 86.603)
            ),
            size=0.25,
            colour="grey"
        ) +

        # -------------------------------------------------------------------- #
        # Add grid labels

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

        # Left lower diamond
        geom_text(
            aes(
                x=c(95, 85, 75, 65),
                y=c(34.6412, 51.9618, 69.2824, 86.603),
                label=c(80, 60, 40, 20)
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
        # Left upper diamond
        geom_text(
            aes(
                x=c(65, 75, 85, 95),
                y=c(121.2442, 138.5648, 155.8854, 173.2060),
                label=c(20, 40, 60, 80)
            ),
            size=2.5
        ) +
        # Right lower diamond
        geom_text(
            aes(
                x=c(125, 135, 145, 155),
                y=c(34.6412, 51.9618, 69.2824, 86.603),
                label=c(80, 60, 40, 20)
            ),
            size=2.5
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
