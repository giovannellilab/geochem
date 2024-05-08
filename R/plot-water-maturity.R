#' Plots the base ternary diagram for water maturity
#' 
#' @param type String indicating whether Giggenbach's or Duchi's coordinates
#' should be used
#' 
#' @return A `ggplot2` object.
#' 
#' @examples
#' plot_base_water_maturity(type="giggenbach")
#' plot_base_water_maturity(type="duchi")
#' 
#' @seealso [geochem::plot_base_gibbs()]
#' @seealso [geochem::plot_base_gigg_anions()]
#' @seealso [geochem::plot_base_ll()]
#' @seealso [geochem::plot_base_major_cations()]
#' @seealso [geochem::plot_base_piper()]
#' 
#' @import ggplot2
#' @import ggtern
#' 
#' @export
plot_base_water_maturity = function(type) {

    # Giggenbach values
    gigg_df = read.csv(
        text="Temperature,Sodium,Potassium,Magnesium,TK
        20,97900,100,109880,tkn
        40,48759,100,12035,tkn
        60,26404,100,1719,tkn
        80,15327,100,306.1,tkn
        100,9431,100,65.6,tkn
        120,6097,100,16.43,tkn
        140,4111,100,4.708,tkn
        160,2885,100,1.514,tkn
        180,2075,100,0.538,tkn
        200,1540,100,0.209,tkn
        220,1170,100,0.0874,tkn
        240,909,100,0.0392,tkn
        260,719,100,0.0187,tkn
        280,579,100,0.0094,tkn
        300,473,100,0.0049,tkn
        320,392,100,0.0027,tkn
        340,329,100,0.0016,tkn
        20,1000,4.6913698876916,243.272178434398,tkm
        40,1000,9.41774637025521,107.30054996903,tkm
        60,1000,17.3882806494252,52.2148201661805,tkm
        80,1000,29.9503828726739,27.5686978576995,tkm
        100,1000,48.6669754009193,15.5873520225527,tkm
        120,1000,75.268939223164,9.3395145645343,tkm
        140,1000,111.599401662987,5.88048364057256,tkm
        160,1000,159.555722917517,3.86416170872552,tkm
        180,1000,221.033850062941,2.63508384781384,tkm
        200,1000,297.878314852584,1.85604816590842,tkm
        220,1000,391.839837478812,1.34502264255868,tkm
        240,1000,504.541406799503,0.999475140308976,tkm
        260,1000,637.452885775713,0.759433137206677,tkm
        280,1000,791.873628347427,0.588616966121383,tkm
        300,1000,968.922251313534,0.464406957635952,tkm
        320,1000,1169.53253313088,0.372311266745217,tkm
        340,1000,1394.45436354981,0.302813961608541,tkm"
    )

    # Choose between Giggenbach and Duchi plots
    if (type == "giggenbach") {
        base_plot = ggtern(
            data=gigg_df,
            aes(
                x=Potassium/100,
                y=Sodium/1000,
                z=sqrt(Magnesium)
            )
        ) +
        labs(
            x="K/100",
            y="Na/1000",
            z=expression(sqrt(Mg)),
            fill="samples"
        )

    } else if (type == "duchi") {
        base_plot = ggtern(
            data=gigg_df,
            aes(
                x=Potassium/10,
                y=Sodium/400,
                z=sqrt(Magnesium)
            )
        ) +
        labs(
            x="K/10",
            y="Na/400",
            z=expression(sqrt(Mg)),
            fill="samples"
        )

    } else {
        stop(
            paste0(
                "Type <", type, "> not implemented in plot_base_water_maturity"
            )
        )
    }

    # Create plot
    plot = base_plot +

        guides(fill=guide_legend(override.aes=list(shape=21))) +

        # Create the first half of the temperature curves
        geom_point(
            data=gigg_df %>% filter(TK %in% c("tkn")),
            size=1,
            shape=22,
            fill="grey",
            stroke=.3
        ) +
        geom_text(
            data=gigg_df %>% filter(TK %in% c("tkn")),
            mapping=aes(label=as.factor(Temperature)),
            color="grey",
            size=2.5,
            hjust=0.5,
            vjust=-1
        ) +

        # Create the second half of the temperature curves
        geom_point(
            data=gigg_df %>% filter(TK %in% c("tkm")),
            size=1,
            shape=22,
            fill="grey",
            stroke=.3
        ) +
        geom_text(
            data=gigg_df %>% filter(TK %in% c("tkm")),
            mapping=aes(label=as.factor(Temperature)),
            color="grey",
            size=2.5,
            hjust=0.5,
            vjust=2
        ) +

        # Add annotations
        annotate(
            "text",
            label="Fully Equilibrated",
            color="grey",
            x=0.2,
            y=0.8,
            z=0.2,
            size=3,
            alpha=1.0,
            fontface="bold"
        ) +
        annotate(
            "text",
            label="Partially Equilibrated",
            color="grey",
            x=0.2,
            y=0.225,
            z=0.2,
            size=3,
            alpha=1.0,
            fontface="bold"
        ) +
        annotate(
            "text",
            label="Immature Waters",
            color="grey",
            x=0.2,
            y=0.05,
            z=0.2,
            size=3,
            alpha=1.0,
            fontface="bold"
        ) +

        # Add theme options
        ggtern::theme_showgrid() +
        ggtern::theme_ticksoutside() +
        ggtern::theme_ticklength_major(major=unit(3.5, "mm"))

    return(plot)
}
