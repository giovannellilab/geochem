test_that("plot_metals returns the expected plot", {
  df = read.csv(test_path("testdata", "processed-data-icp.csv"))
  plot = geochem::plot_metals(df)
  vdiffr::expect_doppelganger(title="", fig=plot)
})
