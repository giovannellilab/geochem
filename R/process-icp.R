library(tidyverse)

remove_less_than = function(x) as.numeric(
    str_replace(
    string=x,
    pattern="<0.000|<LOD",
    replacement="NA"
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

    data_df = data_df %>%
        # Rename columns
        rename(
            sample="Sample Name",
            dilution="Total Dil."
        ) %>%
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
                value_sd=sd(value, na.rm=FALSE)
            )

        measures_df = measures_df %>%
            bind_rows(row_df)
    }

    # Remove unnecessary rows and columns
    measures_df = measures_df %>%
        # Remove RSD rows
        filter(!str_detect(name, "RSD")) %>%
        # Get only concentration rows
        filter(name == "Concentration") %>%
        # Remove name column
        select(-name) %>%
        # Rename value_mean to concentration
        rename(concentration=value_mean)

    # Format element, gas and isotope columns
    measures_df = measures_df %>%
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

    return(measures_df)
}