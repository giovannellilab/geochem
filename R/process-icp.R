#' Replaces <LOD, <0.000 and N/A values in ICP-MS with zeroes
#' 
#' @param x The column in the data frame to be processed.
#' 
#' @return The processed column.
#' 
#' @seealso [geochem::process_icp()]
#' 
#' @importFrom stringr str_replace
.replace_lod_values = function(x) as.numeric(
  stringr::str_replace(
    string=x,
    pattern="<0.000|<LOD|N/A",
    replacement="0.0"
  )
)

#' Processes ICP-MS as given by the instrument
#' 
#' @param filepath String indicating the location of the raw data file.
#' @param blank_name String indicating the name of the blank in the file.
#' 
#' @return A data.frame object.
#' 
#' @examples
#' process_icp(filepath="icp-ms.csv", blank_name="blank")
#' 
#' @seealso [geochem::.replace_lod_values()]
#' @seealso [geochem::select_icp_auto()]
#' @seealso [geochem::plot_metals()]
#' 
#' @importFrom rio import
#' @importFrom stats sd
#' @importFrom tidyr fill pivot_longer pivot_wider
#' @import dplyr
#' @import stringr
#' @import checkmate
#' 
#' @export
process_icp = function(filepath, blank_name) {

  checkmate::assertString(filepath)
  checkmate::assertString(blank_name)

  # Get the element names
  elements_row = rio::import(
    filepath,
    header=FALSE,
    nrows=1
  )

  # Get first row as the element names
  elements_row = elements_row %>%
    slice(1)

  # Fill NaNs for later paste
  elements_row = elements_row %>%
    t() %>%
    as.data.frame() %>%
    tidyr::fill(1, .direction="down") %>%
    t() %>%
    unlist(., use.names=FALSE)

  # Load the whole file
  data_df = rio::import(
    filepath,
    skip=1,
    header=TRUE
  )

  # Paste elements in the column names
  new_cols = paste(
    colnames(data_df),
    elements_row,
    sep="---"
  )
  new_cols = new_cols %>% str_replace("---NA", "")
  colnames(data_df) = new_cols

  # Rename columns
  data_df = data_df %>%
    rename(
      sample="Sample Name",
      dilution="Total Dil."
    )

  # Select all "Conc. RSD" columns (names)
  conc_rsd_cols = data_df %>%
    select(starts_with("Conc. RSD")) %>%
    colnames()

  # Select all "Conc. RSD" columns (indices)
  conc_rsd_cols = which(colnames(data_df) %in% conc_rsd_cols)

  # Assume previous column is the one with element concentration
  conc_rsd_cols_all = sort(
    c(
      conc_rsd_cols - 1,
      conc_rsd_cols,
      conc_rsd_cols + 1,
      conc_rsd_cols + 2
    )
  )

  measures_df = data.frame()

  # Iterate 4 by 4 (step) to get all columns for the respective element
  step = 4

  for (col_idx in seq(1, length(conc_rsd_cols_all), by=step)) {

    # Get name of the element
    selected_cols = conc_rsd_cols_all[col_idx:(col_idx + step - 1)]
    selected_cols_names = data_df %>%
      select(all_of(selected_cols)) %>%
      colnames()
    element_name = str_split_i(
      string=selected_cols_names[[1]],
      pattern="---",
      i=2
    )

    row_df = data_df %>%
      # Select columns from data_df
      select(
        c(
          sample,
          dilution,
          all_of(selected_cols)
        )
      ) %>%
      # Replace "less than" values by NA
      mutate_at(
        selected_cols_names,
        .replace_lod_values
      ) %>%
      # Round dilutions to match them
      mutate(dilution=round(dilution, digits=0))

    row_df = row_df %>%
      # Create element column (unformatted)
      mutate(element=rep(element_name, n=length(row_df))) %>%
      # Transform to long format
      tidyr::pivot_longer(
        cols=-c(sample, dilution, element),
        names_to="measurement"
      )

    measures_df = measures_df %>%
      bind_rows(row_df)
  }

  # Format sample, measurement, element, gas and isotope columns
  measures_df = measures_df %>%
    # Extract and replace replicates inside the sample name
    mutate(
      replicate=str_split_i(
        string=sample,
        pattern="_",
        i=4
      ),
      sample=str_replace(
        string=sample,
        pattern="_1in\\d+_\\d+$",
        replacement=""
      )
    ) %>%
    # Remove unnecessary characters in measurement column
    mutate(
      measurement=str_split_i(
        string=measurement,
        pattern="---",
        i=1
      )
    ) %>%
    mutate(
      measurement=str_replace(
        string=measurement,
        pattern="\\.\\.\\.\\d+$",
        replacement=""
      )
    ) %>%
    # Remove leading characters from element column
    mutate(
      element=str_replace(element, "Conc. \\[ ppb \\] ", "")
    ) %>%
    # Remove leading characters from element column (another format)
    mutate(
      element=str_replace(
        string=element,
        pattern="Conc\\. \\[ ppb \\]\\.\\.\\.\\d+ ",
        replacement=""
      )
    ) %>% 
    # Extract gas column (https://stackoverflow.com/a/61296180)
    mutate(
      gas=str_extract(
        string=element,
        pattern="(?<=\\[\\s).*(?=\\s\\]$)"
      )
    ) %>%
    # Extract isotope from the element column
    mutate(
      isotope=str_extract(
        string=element,
        pattern="^\\d+(?= .*)"
      )
    ) %>%
    # Extract actual element name from the element column
    mutate(
      element=str_extract(
        string=element,
        pattern="(?<=[\\d+] ).*(?= \\[.*\\])"
      )
    ) %>%
    mutate(
      element=str_replace_all(
        string=element,
        pattern=" ",
        replacement=""
      )
    )

  measures_df_raw = measures_df %>%
    # Convert to wide for better readability
    tidyr::pivot_wider(
      id_cols=c(sample, dilution, replicate, element, isotope, gas),
      names_from=measurement,
      values_from=value
    ) %>%
    arrange(sample, dilution, element, isotope, gas) %>%

    # NOTE: do these calculations here to avoid removal by summarise!
    # Exclude dilution to calculate dilution change
    group_by(sample, element, isotope, gas) %>%
    # Calculate dilution change for further checks
    mutate(dilution_change=dilution/min(dilution)) %>%
    mutate(CPS_adj=CPS * dilution_change) %>%
    mutate(
      CPS_perc_change=100 * (abs(CPS_adj - lag(CPS_adj)) / lag(CPS_adj))
    ) %>%
    mutate(
      CPS_perc_check=case_when(
        CPS_perc_change <= 5.0 ~ "OK",
        CPS_perc_change >  5.0 ~ "DISCARD"
      )
    )

  measures_df = measures_df %>%
    # Select only concentration columns
    dplyr::filter(measurement == "Conc. [ ppb ]") %>%
    select(-measurement) %>%
    # Exclude replicate to calculate the mean
    group_by(sample, dilution, element, isotope, gas) %>%
    # Calculate mean and standard deviation
    summarise(
      .groups="keep",
      concentration=mean(value, na.rm=TRUE),
      conc_sd=stats::sd(value, na.rm=FALSE)
    ) %>%
    # Add checks for standard deviation
    mutate(conc_sd_perc=100 * conc_sd / concentration) %>%
    mutate(
      conc_sd_check=case_when(
        conc_sd_perc >=  0.0 & conc_sd_perc <= 15.0 ~ "OK",
        conc_sd_perc >  15.0 & conc_sd_perc <= 30.0 ~ "CHECK",
        conc_sd_perc >  30.0 | is.na(conc_sd_perc)  ~ "DISCARD"
      )
    ) %>%
    # Add checks for calibration curve
    mutate(
      cal_curve_check=case_when(
        concentration >= 0.01 & concentration <= 100.0 ~ "OK",
        concentration < 0.01  ~ "BELOW",
        concentration > 100.0 ~ "ABOVE"
      )
    )

  # Merge both CPS and SD checks
  measures_df = left_join(
    x=measures_df_raw,
    y=measures_df,
    by=c("sample", "dilution", "element", "isotope", "gas")
  )

  # Sort samples and put blank first
  sample_order = c(
    blank_name,
    measures_df %>%
      dplyr::filter(sample != blank_name) %>%
      pull(sample) %>%
      unique()
  )
  measures_df = measures_df %>%
    mutate(sample=factor(sample, levels=sample_order)) %>%
    arrange(sample)

  return(measures_df)
}

#' Automatically selects the processed ICP-MS data according to defined checks
#' 
#' @param df Data frame as given by [geochem::process_icp()] function
#' @param blank_name String indicating the name of the blank in the file.
#' 
#' @return A data.frame object.
#' 
#' @examples
#' select_icp_auto(df=df, blank_name="blank")
#' 
#' @seealso [geochem::process_icp()]
#' 
#' @import dplyr
#' @import checkmate
#' 
#' @export
select_icp_auto = function(df, blank_name) {
  
  checkmate::assertDataFrame(
    x=df,
    col.names="named",
    ncols=19
  )
  checkmate::assertSetEqual(
    x=colnames(df),
    y=c(
      "sample",
      "dilution",
      "replicate",
      "element",
      "isotope",
      "gas",
      "Conc. [ ppb ]",
      "Conc. RSD",
      "CPS",
      "CPS RSD",
      "dilution_change",
      "CPS_adj",
      "CPS_perc_change",
      "CPS_perc_check",
      "concentration",
      "conc_sd",
      "conc_sd_perc",
      "conc_sd_check",
      "cal_curve_check"
    )
  )
  checkmate::assertString(blank_name)

  # Automatically select values that passed the checks
  final_df = df %>%
    # Discard values that didn't pass the checks (keep blank)
    dplyr::filter(
      (
        CPS_perc_check != "DISCARD" & sample != blank_name |
        sample == blank_name
      ),
      (
        conc_sd_check != "DISCARD" & sample != blank_name |
        sample == blank_name
      ),
      (
        cal_curve_check == "OK" & sample != blank_name |
        sample == blank_name
      )
    )

  return(final_df)
}
