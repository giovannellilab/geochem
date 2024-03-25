library(tidyverse)

plot_ll = function(df) {

    # Add anions and cations transformations as columns
    df = df %>%
        mutate(
            r_bicarb=50 * (HCO3.meq) / sum(
                HCO3.meq + Cl.meq + SO4.meq
            ),
            r_ca_mg=50 * (Mg.meq + Ca.meq) / sum(
                Na.meq + K.meq + Mg.meq + Ca.meq
            )
        ) %>%
        mutate(
            r_na_k=50 - r_ca_mg,
            r_cl_so4=50 - r_bicarb
        )

    ggplot(data=df) + 
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
        )
}
