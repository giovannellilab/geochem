test_that("plot_base_piper returns a ggplot2 object", {
  plot = geochem::plot_base_piper()
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_piper returns the expected plot", {
  plot = geochem::plot_base_piper()
  vdiffr::expect_doppelganger(title="", fig=plot)
})

test_that("transform_data_piper returns the same data.frame", {
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
  
  # Force conversion of SiteID to factor
  ic_df = ic_df %>% mutate(SiteID=as.factor(SiteID))
  
  piper_data = geochem::transform_data_piper(
      Mg=ic_df$Mg.meq.perc,
      Ca=ic_df$Ca.meq.perc,
      Cl=ic_df$Cl.meq.perc,
      SO4=ic_df$SO4.meq.perc,
      id=ic_df$SiteID
  )
  piper_data = piper_data %>% rename(SiteID=id)
  piper_data_expected = read.csv(
    test_path("testdata", "ic-data-piper.csv"),
    stringsAsFactors=TRUE
  )
  
  testthat::expect_equal(
    object=piper_data,
    expected=piper_data_expected
  )
})
