library(tidyverse)

process_icp_data = function(filepath) {

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

    step = 4

    for (col_idx in seq(1, length(conc_rsd_cols_all), by=step)) {
        # Get name of the element
        selected_cols = conc_rsd_cols_all[col_idx:(col_idx + step - 1)]
        selected_cols_names = data_df %>%
            select(all_of(selected_cols)) %>%
            colnames()
        element_name = selected_cols_names[[1]]

        # Defined function for less than
        remove_less_than = function(x) as.numeric(
            str_replace(
            string=x,
            pattern="<0.000|<LOD",
            replacement="NA"
            )
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
            # Remove "less than" values
            mutate_at(
                selected_cols_names,
                remove_less_than
            ) %>%
            # Round dilutions to match them
            mutate(dilution=round(dilution, digits=0))

        row_df = row_df %>% 
            mutate(element=rep(element_name, n=length(row_df))) %>%
            rename(Concentration=eval(element_name))
        
        row_df = row_df %>%
            # Transform to long format
            pivot_longer(
                cols=-c(sample, dilution, element),
                names_to="name"
            ) %>%
            # Calculate mean
            group_by(sample, dilution, name, element) %>%
            summarise(
                .groups="keep",
                value_mean=mean(value, na.rm=TRUE),
                value_sd=sd(value, na.rm=TRUE)
            )

        # Add as SD the value from RSD since there are no replicates
        row_df = row_df %>%
            mutate(value_sd=ifelse(
            str_detect(string=name, pattern="RSD"),
                value_mean,
                value_sd
            )) %>%
            group_by(sample, dilution, element) %>%
            fill(value_sd, .direction="up") %>%
            fill(value_sd, .direction="down")

        measures_df = measures_df %>%
            bind_rows(row_df)
    }

    # Add checks to CV
    measures_df = measures_df %>%
        mutate(
            value_sd_check=case_when(
                value_sd >=  0.0 & value_sd <= 15.0 ~ "OK",
                value_sd >  15.0 & value_sd <= 30.0 ~ "CHECK",
                value_sd >  30.0 ~ "DISCARD"
            )
        )

    measures_df = measures_df %>%
        mutate(
            element=str_replace(element, "Conc. \\[ ppb \\] ", "")
        ) %>%
        # Remove RSD rows
        filter(!str_detect(name, "RSD"))

    # Calculate dilution change for further checks
    measures_df = measures_df %>%
        arrange(sample, dilution) %>%
        group_by(sample, name) %>%
        mutate(dilution_change=dilution/max(dilution)) %>%
        mutate(CPS_adj=value_mean * dilution_change) %>%
        mutate(CPD_perc_change=100*(abs(CPS_adj - lag(CPS_adj))/lag(CPS_adj))) %>%
        mutate(
            value_cps_perc_check=case_when(
                CPD_perc_change <= 5.0 ~ "OK",
                CPD_perc_change >  5.0 ~ "DISCARD"
            )
        )

    measures_df = measures_df %>%
        # Remove leading characters from element column
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
                pattern="(?<=\\[).*(?=\\]$)"
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
        ) %>%
        # Get only concentration rows
        filter(name == "Concentration") %>%
        rename(concentration=value_mean)

    return(measures_df)
}
