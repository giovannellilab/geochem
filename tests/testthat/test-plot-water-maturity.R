test_that("plot_base_water_maturity (Giggenbach) returns a ggplot2 object", {
  plot = geochem::plot_base_water_maturity(type="giggenbach")
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_water_maturity (Duchi) returns a ggplot2 object", {
  plot = geochem::plot_base_water_maturity(type="duchi")
  expect_true(ggplot2::is.ggplot(plot))
})

test_that("plot_base_water_maturity (Giggenbach) returns the expected plot", {
  # NOTE: just run once to avoid overwriting the snapshot
  plot = geochem::plot_base_water_maturity(type="giggenbach")
  vdiffr::expect_doppelganger(title="", fig=plot)
})

test_that("plot_base_water_maturity fails when type is not accepted", {
  testthat::expect_error(
    object=geochem::plot_base_water_maturity(type="other")
  )
})
