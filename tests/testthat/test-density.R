test_that("generate_density_plot returns ggplot object", {
  prep <- prep_data("hurricane")
  df <- prep[[1]]
  p <- generate_density_plot(df, "edif", "Excess Fatalities")
  expect_s3_class(p, "ggplot")
})
