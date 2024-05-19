test_that("plot_base_gibbs returns a ggplot2 object", {
  plot = geochem::plot_base_gibbs()
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_gibbs returns the expected plot", {
  plot = geochem::plot_base_gibbs()
  vdiffr::expect_doppelganger(title="", fig=plot)
})
