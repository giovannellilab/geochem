test_that("plot_base_ll returns a ggplot2 object", {
  plot = geochem::plot_base_ll()
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_ll returns the expected plot", {
  plot = geochem::plot_base_ll()
  vdiffr::expect_doppelganger(title="", fig=plot)
})
