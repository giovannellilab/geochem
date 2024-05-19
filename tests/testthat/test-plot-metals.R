test_that("plot_metals returns the expected plot", {
  df = read.csv(test_path("testdata", "icp-data-plot.csv"))
  plot = geochem::plot_metals(df)
  vdiffr::expect_doppelganger(title="", fig=plot)
})
