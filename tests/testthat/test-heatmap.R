test_that("generate_heatmap_strips returns list of ggplot objects", {
  prep <- prep_data("hurricane")
  df <- prep[[1]]
  strip_vars <- prep[[4]]
  variable_labels <- prep[[5]]

  strips <- generate_heatmap_strips(df, "edif", strip_vars, variable_labels)

  expect_type(strips, "list")
  expect_s3_class(strips[[1]], "ggplot")
})
