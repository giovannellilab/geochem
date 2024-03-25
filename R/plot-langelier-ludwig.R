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

    plot = ggplot(
            data=df,
            aes(
                x=r_bicarb,
                y=r_na_k,
                color=ID
            )
        ) + 
        geom_point() +
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
        )

    return(plot)
}
