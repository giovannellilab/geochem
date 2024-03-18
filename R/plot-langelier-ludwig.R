plot_ll = function(df) {

    # Add anions and cations transformations as columns
    df = df %>%
        mutate(
            axis_anions=sqrt(2/3) * log(HCO3.meq / (Cl.meq * SO4.meq)),
            axis_cations=log((Na.meq * K.meq) / (Ca.meq * Mg.meq))
        )

    ggplot(data=df) + 
        geom_point(aes(x=axis_anions, y=axis_cations, color=ID)) +
        labs(
            x=expression(
                sqrt(frac(2, 3)) * log(frac("HCO"[3]^"-", "Cl"^"-" * "SO"[4]^"2-"))
            ),
            y=expression(log(frac("Na"^"+" * "K"^"+", "Ca"^"2+" * "Mg"^"2+")))
        )
}
