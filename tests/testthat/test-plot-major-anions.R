test_that("plot_base_major_anions returns a ggplot2 object", {
  plot = geochem::plot_base_major_anions()
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_major_anions returns the expected plot", {
  plot = geochem::plot_base_major_anions()
  vdiffr::expect_doppelganger(title="", fig=plot)
})
