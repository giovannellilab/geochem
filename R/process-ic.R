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
#' @examples
#' df = process_ic(df)
#' 
#' @seealso [geochem::plot_base_gibbs()]
#' @seealso [geochem::plot_base_gigg_anions()]
#' @seealso [geochem::plot_base_ll()]
#' @seealso [geochem::plot_base_major_cations()]
#' @seealso [geochem::plot_base_piper()]
#' @seealso [geochem::plot_base_water_maturity()]
#' 
#' @import dplyr
#' @importFrom tidyr replace_na
#' 
#' @export
process_ic = function(df) {

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

  ic_df = df %>%
    # Fill NA (missing ions)
    mutate(across(all_of(ion_cols), ~tidyr::replace_na(.x, 0))) %>%
    # WARNING: Calculate meq for plotting, concentrations must be in mg/L!!!
    mutate(
      # Anions
      HCO3.meq=alk_tot * (1 / 61.0168) * 1.22,
      Cl.meq=cl * (1 / 35.45),
      SO4.meq=so4 * (2 / 96.06),
      Br.meq=br * (1 / 79.904),
      # NO2.meq=no2 * (1 / 46.005),
      NO3.meq=no3 * (1 / 62.004),
      P04.meq=po4 * (3 / 94.9714),

      # Cations
      Na.meq=na * (1 / 22.990),
      K.meq=k * (1 / 39.098),
      Ca.meq=ca * (2 / 40.078),
      Mg.meq=mg * (2 / 24.305),
      NH4.meq=nh4 * (1 / 18.039),
      Li.meq=li * (1 / 6.94)
    ) %>%
    # Calculate ion balance
    mutate(
      sum_anions=(
        HCO3.meq + Cl.meq + SO4.meq + Br.meq + NO3.meq + P04.meq
      ),
      sum_cations=(
        Na.meq + K.meq + Ca.meq + Mg.meq + NH4.meq + Li.meq
      )
    ) %>%
    mutate(
      IB=100 * (sum_cations - sum_anions) / (sum_cations + sum_anions)
    )

  # Langelier-Ludwig: add anions and cations transformations as columns
  ic_df = ic_df %>%
    mutate(
      r_bicarb=50 * (HCO3.meq) / (HCO3.meq + Cl.meq + SO4.meq),
      r_na_k=50 * (Na.meq + K.meq) / (Na.meq + K.meq + Mg.meq + Ca.meq)
    ) %>%
    mutate(
      r_ca_mg=50 - r_na_k,
      r_cl_so4=50 - r_bicarb
    )

  # Piper plot: calculate percentages
  ic_df = ic_df %>%
    mutate(
      piper_cations=(Mg.meq + Ca.meq + Na.meq + K.meq),
      piper_anions=(HCO3.meq + SO4.meq + Cl.meq)
    ) %>%
    mutate(
      Mg.meq.perc=100 * Mg.meq / piper_cations,
      Ca.meq.perc=100 * Ca.meq / piper_cations,
      Cl.meq.perc=100 * Cl.meq / piper_anions,
      SO4.meq.perc=100 * SO4.meq / piper_anions,
    )

  return(ic_df)
}
