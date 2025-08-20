test_that("plot_multiverse returns patchwork object", {
  p <- plot_multiverse("hurricane")
  expect_s3_class(p, "gg")
  expect_true("patchwork" %in% class(p))
})

test_that("plot_multiverse can save output", {
  tmp <- tempfile(fileext = ".png")
  plot_multiverse("hurricane", output_path = tmp)
  expect_true(file.exists(tmp))
})
