library(tidyverse)

process_ic_data = function(df) {

    df = df %>%
        # Remove time column
        select(-`Determination start`) %>%
        # Rename ID column
        rename(ID=Ident)

    # Merge anions and cations (see https://stackoverflow.com/a/45518649)
    df = df %>%
        group_by(ID) %>%
        fill(everything(), .direction = "downup") %>%
        slice(1)

    # Rename columns (see https://stackoverflow.com/a/23518906)

    # Create the mapping for renaming the columns
    columns_map = as.vector(colnames(df))
    names(columns_map) = str_extract(
        string=colnames(df),
        pattern="(?<=[Anions|Cations]\\.)(.+)(?=\\.Concentration)"
    )

    # Deselect the original columns
    columns_map = columns_map[!is.na(names(columns_map))]

    # Finally, rename the columns using the mapping
    df = df %>%
        rename(all_of(columns_map))

    return(df)
}
