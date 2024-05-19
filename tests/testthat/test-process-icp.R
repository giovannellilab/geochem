test_that("process_icp returns the same data.frame", {
  ICP_FILENAME = "PROJECT_NAME.xlsx"
  ICP_BLANK_NAME = "BLANK"
  ICP_DIR = system.file("extdata", "PROJECT_NAME/icp-ms", package="geochem")
  
  icp_df_check = suppressMessages(
     process_icp(
      filepath=file.path(ICP_DIR, ICP_FILENAME),
      blank_name=ICP_BLANK_NAME
    )
  )
  icp_df_check_expected = read.csv(test_path("testdata", "icp-data-check.csv"))
  
  # Rename formatted columns while importing
  icp_df_check_expected = icp_df_check_expected %>%
    dplyr::rename(
      `Conc. [ ppb ]`=`Conc....ppb..`,
      `Conc. RSD`=`Conc..RSD`,
      `CPS RSD`=`CPS.RSD`
    )
  
  testthat::expect_equal(
    object=colnames(icp_df_check),
    expected=colnames(icp_df_check_expected)
  )
})
