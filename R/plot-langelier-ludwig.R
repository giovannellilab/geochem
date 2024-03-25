library(tidyverse)

plot_ll = function(df) {

    # Add anions and cations transformations as columns
    df = df %>%
        mutate(
            r_bicarb=50 * (HCO3.meq) / sum(
                HCO3.meq + Cl.meq + SO4.meq
            ),
            r_na_k=50 * (Na.meq + K.meq) / sum(
                Na.meq + K.meq + Mg.meq + Ca.meq
            )
        ) %>%
        mutate(
            r_ca_mg=50 - r_na_k,
            r_cl_so4=50 - r_bicarb
        )

    plot = ggplot(data=df) + 
        geom_point(
            aes(
                x=r_bicarb,
                y=r_na_k,
                color=ID
            )
        ) +
        labs(
            x=expression("R"("HCO"[3]^"-")),
            y=expression("R"("Na"^"+" + "K"^"+"))
        ) +
        scale_x_continuous(
            expression("R"("Cl"^"-" + "SO"[4]^"2-")),
            sec.axis=sec_axis(r_cl_so4~., name=derive())
        ) +
        scale_y_continuous(
            expression("R"("Ca"^"2+" + "Mg"^"2+")),
            sec.axis=sec_axis(r_ca_mg~., name=derive())
        )

    return(plot)
}
