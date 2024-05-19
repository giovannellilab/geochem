test_that("process_ic returns the same data.frame", {
  data_df = rio::import(
    file=file.path(
      system.file("extdata", "PROJECT_NAME", package="geochem"),
      paste0("PROJECT_NAME", "_env_dataset.xlsx")
    ),
    sheet="envdata",
    skip=1
  )
  
  # Remove first row containing the units
  data_df = data_df %>% dplyr::slice(2:nrow(data_df))
  
  # Convert data columns to numeric
  data_df = utils::type.convert(data_df, as.is=TRUE)
  
  ic_df = geochem::process_ic(data_df)
  ic_df_expected = read.csv(test_path("testdata", "ic-data-processed.csv"))
  
  testthat::expect_equal(
    object=ic_df,
    expected=ic_df_expected
  )
})
