set.seed(123)
resample_idx <- replicate(
  n = 10,
  expr = sample.int(
    n = nrow(yardstick::two_class_example),
    size = 300,
    replace = TRUE
  ),
  simplify = FALSE
)

two_class_resamples <- dplyr::bind_rows(
  lapply(resample_idx, function(idx) yardstick::two_class_example[idx,]),
  .id = "Resample"
) %>%
  dplyr::group_by(Resample)

test_that("calib_curve - two class", {
  res <- calib_curve(yardstick::two_class_example, truth = truth, predictions = Class1)
  expect_s3_class(res, "calib_df")
})

test_that("calib_curve - two class, with resamples", {
  res <- calib_curve(two_class_resamples, truth = truth, predictions = Class1)
  expect_s3_class(res, "calib_df")
})

test_that("calib_curve autoplot", {
  res <- calib_curve(yardstick::two_class_example, truth = truth, predictions = Class1)
  expect_error(.plot <- ggplot2::autoplot(res), NA)
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # x and y data
  expect_equal(res$.estimate, .plot_data$data[[2]]$x)
  expect_equal(res$.truth, .plot_data$data[[2]]$y)
})

test_that("calib_curve - autoplot, with resamples", {
  res <- calib_curve(two_class_resamples, truth = truth, predictions = Class1)
  expect_error(.plot <- ggplot2::autoplot(res), NA)
  expect_s3_class(.plot, "gg")

  .plot_data <- ggplot2::ggplot_build(.plot)

  # x and y data
  expect_equal(res$.estimate, .plot_data$data[[2]]$x)
  expect_equal(res$.truth, .plot_data$data[[2]]$y)
})

test_that("`event_level = 'second'` works", {
  df <- yardstick::two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    calib_curve_vec(df$truth, df$Class1),
    calib_curve_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )

  expect_equal(
    calib_curve_vec(df$truth, df$Class1),
    calib_curve_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})
