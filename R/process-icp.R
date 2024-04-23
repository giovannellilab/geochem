library(tidyverse)

replace_lod_values = function(x) as.numeric(
    str_replace(
        string=x,
        pattern="<0.000|<LOD|N/A",
        replacement=0.0
    )
)

process_icp = function(filepath) {

    # Get the element names
    elements_row = import(
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
        fill(1, .direction="down") %>%
        t() %>%
        unlist(., use.names=FALSE)

    # Load the whole file
    data_df = import(
        filepath,
        skip=1,
        header=TRUE
    )

    # Paste elements in the column names
    new_cols = paste(
        colnames(data_df),
        elements_row
    )
    new_cols = new_cols %>% str_replace(" NA", "")
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
        element_name = selected_cols_names[[1]]

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
                replace_lod_values
            ) %>%
            # Round dilutions to match them
            mutate(dilution=round(dilution, digits=0))

        row_df = row_df %>%
            # Create element column (unformatted)
            mutate(element=rep(element_name, n=length(row_df))) %>%
            # Transform to long format
            pivot_longer(
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
                pattern="\\.\\.\\.",
                i=1
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
        pivot_wider(
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
        filter(measurement == "Conc. [ ppb ]") %>%
        select(-measurement) %>%
        # Exclude replicate to calculate the mean
        group_by(sample, dilution, element, isotope, gas) %>%
        # Calculate mean and standard deviation
        summarise(
            .groups="keep",
            conc_mean=mean(value, na.rm=TRUE),
            conc_sd=sd(value, na.rm=FALSE)
        ) %>%
        # Add checks for standard deviation
        mutate(conc_sd_perc=100 * conc_sd / conc_mean) %>%
        mutate(
            conc_sd_check=case_when(
                conc_sd_perc >=  0.0 & conc_sd_perc <= 15.0 ~ "OK",
                conc_sd_perc >  15.0 & conc_sd_perc <= 30.0 ~ "CHECK",
                conc_sd_perc >  30.0 | is.na(conc_sd_perc)  ~ "DISCARD"
            )
        )

    # Merge both CPS and SD checks
    measures_df = left_join(
        x=measures_df_raw,
        y=measures_df,
        by=c("sample", "dilution", "element", "isotope", "gas")
    )

    return(measures_df)
}

select_icp_auto = function(df, blank_name) {
    # Automatically select values that passed the checks
    final_df = df %>%
        # Discard values that didn't pass the checks (keep blank)
        filter(
            (
                CPS_perc_check != "DISCARD" & sample != blank_name |
                sample == blank_name
            ),
            (
                conc_sd_check != "DISCARD" & sample != blank_name |
                sample == blank_name
            ),
        ) %>%
        # Get unique values in conc_mean: there are duplicates because the
        # original data points are kept for the CPS_perc_check
        select(sample, dilution, element, isotope, gas, conc_mean) %>%
        distinct() %>%
        # Rename concentration column for further plots
        rename(concentration=conc_mean)

    return(final_df)
}
