test_that("plot_base_major_cations returns a ggplot2 object", {
  plot = geochem::plot_base_major_cations()
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_major_cations returns the expected plot", {
  plot = geochem::plot_base_major_cations()
  vdiffr::expect_doppelganger(title="", fig=plot)
})
