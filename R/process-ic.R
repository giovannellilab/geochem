#' Processes IC results for further geochemistry plots:
#' * Calculates miliequivalents for the given species
#' * Adds coordinate transformations for the Langelier-Ludwig diagram
#' * Adds percentage transformations for the Piper plot
#' 
#' @param df Data frame containing the following columns:
#' * Anions: alk_tot, cl, so4, br, no3, po4
#' * Cations: na, k, ca, mg, nh4, li
#' 
#' @return A data.frame object.
#' 
#' @seealso [geochem::plot_base_gibbs()]
#' @seealso [geochem::plot_base_ll()]
#' @seealso [geochem::plot_base_major_anions()]
#' @seealso [geochem::plot_base_major_cations()]
#' @seealso [geochem::plot_base_piper()]
#' @seealso [geochem::plot_base_water_maturity()]
#' 
#' @import dplyr
#' @importFrom tidyr replace_na
#' @import checkmate
#' 
#' @export
#' 
process_ic = function(df) {

  checkmate::assertDataFrame(
    x=df,
    col.names="named"
  )

  ion_cols = c(
    # Anions
    "alk_tot",
    "cl",
    "so4",
    "br",
    "no3",
    "po4",

    # Cations
    "na",
    "k",
    "ca",
    "mg",
    "nh4",
    "li"
  )

  # Check if the required columns are in the dataframe
  for (col in ion_cols) {
    checkmate::assertChoice(
      x=col,
      choices=colnames(df)
    )
  }

  ic_df = df %>%
    # Fill NA (missing ions)
    mutate(across(all_of(ion_cols), ~tidyr::replace_na(.x, 0))) %>%
    # WARNING: Calculate meq for plotting, concentrations must be in mg/L!!!
    mutate(
      # Anions
      HCO3.meq=get("alk_tot") * (1 / 61.0168) * 1.22,
      Cl.meq=get("cl") * (1 / 35.45),
      SO4.meq=get("so4") * (2 / 96.06),
      Br.meq=get("br") * (1 / 79.904),
      # NO2.meq=no2 * (1 / 46.005),
      NO3.meq=get("no3") * (1 / 62.004),
      P04.meq=get("po4") * (3 / 94.9714),

      # Cations
      Na.meq=get("na") * (1 / 22.990),
      K.meq=get("k") * (1 / 39.098),
      Ca.meq=get("ca") * (2 / 40.078),
      Mg.meq=get("mg") * (2 / 24.305),
      NH4.meq=get("nh4") * (1 / 18.039),
      Li.meq=get("li") * (1 / 6.94)
    ) %>%
    # Calculate ion balance
    mutate(
      sum_anions=(
        get("HCO3.meq") + get("Cl.meq") + get("SO4.meq") + 
        get("Br.meq") + get("NO3.meq") + get("P04.meq")
      ),
      sum_cations=(
        get("Na.meq") + get("K.meq") + get("Ca.meq") + 
        get("Mg.meq") + get("NH4.meq") + get("Li.meq")
      )
    ) %>%
    mutate(
      IB=100 * 
      (get("sum_cations") - get("sum_anions")) / 
      (get("sum_cations") + get("sum_anions"))
    )

  # Langelier-Ludwig: add anions and cations transformations as columns
  ic_df = ic_df %>%
    mutate(
      r_bicarb=50 * (get("HCO3.meq")) / 
        (get("HCO3.meq") + get("Cl.meq") + get("SO4.meq")),
      r_na_k=50 * (get("Na.meq") + get("K.meq")) / 
        (get("Na.meq") + get("K.meq") + get("Mg.meq") + get("Ca.meq"))
    ) %>%
    mutate(
      r_ca_mg=50 - get("r_na_k"),
      r_cl_so4=50 - get("r_bicarb")
    )

  # Piper plot: calculate percentages
  ic_df = ic_df %>%
    mutate(
      piper_cations=(
        get("Mg.meq") + get("Ca.meq") + get("Na.meq") + get("K.meq")
      ),
      piper_anions=(
        get("HCO3.meq") + get("SO4.meq") + get("Cl.meq")
      )
    ) %>%
    mutate(
      Mg.meq.perc=100 * get("Mg.meq") / get("piper_cations"),
      Ca.meq.perc=100 * get("Ca.meq") / get("piper_cations"),
      Cl.meq.perc=100 * get("Cl.meq") / get("piper_anions"),
      SO4.meq.perc=100 * get("SO4.meq") / get("piper_anions"),
    )

  return(ic_df)
}
