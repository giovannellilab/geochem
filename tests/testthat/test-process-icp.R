test_that("process_icp returns the same data.frame", {
  ICP_FILENAME = "PROJECT_NAME.xlsx"
  ICP_BLANK_NAME = "BLANK"
  ICP_DIR = system.file("extdata", "PROJECT_NAME/icp-ms", package="geochem")
  
  icp_df_check = suppressMessages(
    geochem::process_icp(
      filepath=file.path(ICP_DIR, ICP_FILENAME),
      blank_name=ICP_BLANK_NAME
    )
  )
  icp_df_expected = read.csv(test_path("testdata", "icp-data-check.csv"))
  
  # Rename formatted columns while importing
  icp_df_expected = icp_df_expected %>%
    dplyr::rename(
      `Conc. [ ppb ]`=`Conc....ppb..`,
      `Conc. RSD`=`Conc..RSD`,
      `CPS RSD`=`CPS.RSD`
    ) %>%
    dplyr::mutate(
      sample=as.factor(sample),
      replicate=as.character(replicate),
      isotope=as.character(isotope)
    )
  
  testthat::expect_equal(
    object=icp_df_check,
    expected=icp_df_expected
  )
})

test_that("select_icp_auto returns the same data.frame", {
  ICP_FILENAME = "PROJECT_NAME.xlsx"
  ICP_BLANK_NAME = "BLANK"
  ICP_DIR = system.file("extdata", "PROJECT_NAME/icp-ms", package="geochem")
  
  icp_df_check = suppressMessages(
    geochem::process_icp(
      filepath=file.path(ICP_DIR, ICP_FILENAME),
      blank_name=ICP_BLANK_NAME
    )
  )
  icp_df_auto = geochem::select_icp_auto(
    df=icp_df_check,
    blank_name=ICP_BLANK_NAME
  )
  icp_df_expected = read.csv(test_path("testdata", "icp-data-auto.csv"))
  
  # Rename formatted columns while importing
  icp_df_expected = icp_df_expected %>%
    dplyr::rename(
      `Conc. [ ppb ]`=`Conc....ppb..`,
      `Conc. RSD`=`Conc..RSD`,
      `CPS RSD`=`CPS.RSD`
    ) %>%
    dplyr::mutate(
      # Add empty factor levels (removed in select_icp_auto)
      sample=factor(sample, levels=levels(icp_df_check$sample)),
      replicate=as.character(replicate),
      isotope=as.character(isotope)
    )
  
  testthat::expect_equal(
    object=icp_df_auto,
    expected=icp_df_expected
  )
})
