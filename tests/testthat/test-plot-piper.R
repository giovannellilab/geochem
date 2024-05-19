test_that("plot_base_piper returns a ggplot2 object", {
  plot = geochem::plot_base_piper()
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_piper returns the expected plot", {
  plot = geochem::plot_base_piper()
  vdiffr::expect_doppelganger(title="", fig=plot)
})
