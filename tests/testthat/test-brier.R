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


test_that("brier and sbrier - two class", {
  df <- yardstick::two_class_example
  expect_equal(brier_vec(df$truth, df$Class1), brier(df, truth, Class1)$.estimate)
  expect_equal(sbrier_vec(df$truth, df$Class1), sbrier(df, truth, Class1)$.estimate)
})

test_that("brier and sbrier - two class, resampled", {
  brier_resampled <- brier(two_class_resamples, truth, Class1)
  sbrier_resampled <- sbrier(two_class_resamples, truth, Class1)
  expect_equal(colnames(brier_resampled), colnames(sbrier_resampled))
})

test_that("`event_level = 'second'` works", {
  df <- yardstick::two_class_example

  df_rev <- df
  df_rev$truth <- stats::relevel(df_rev$truth, "Class2")

  expect_equal(
    brier_vec(df$truth, df$Class1),
    brier_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )

  expect_equal(
    sbrier_vec(df$truth, df$Class1),
    sbrier_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})
