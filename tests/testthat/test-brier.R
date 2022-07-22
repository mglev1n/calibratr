test_that("brier_vec works", {
  observations <- factor(c("CASE", "CONTROL", "CASE", "CONTROL", "CONTROL"))
  predictions <- c(0, 0.125, 0.9, 0, 0.67)
  expect_equal(brier_vec(observations, predictions), 0.294905, tolerance = 0.001)
})

test_that("sbrier_vec works", {
  observations <- factor(c("CASE", "CONTROL", "CASE", "CONTROL", "CONTROL"))
  predictions <- c(0, 0.125, 0.9, 0, 0.67)
  expect_equal(sbrier_vec(observations, predictions), -0.2287708, tolerance = 0.001)
})

test_that("brier works", {
  observations <- factor(c("CASE", "CONTROL", "CASE", "CONTROL", "CONTROL"))
  predictions <- c(0, 0.125, 0.9, 0, 0.67)
  df <- tibble(observations, predictions)
  brier.df <- brier(df, truth = observations, estimate = predictions)
  expect_equal(brier.df$.estimate, 0.294905, tolerance = 0.001)
})

test_that("sbrier works", {
  observations <- factor(c("CASE", "CONTROL", "CASE", "CONTROL", "CONTROL"))
  predictions <- c(0, 0.125, 0.9, 0, 0.67)
  df <- tibble(observations, predictions)
  sbrier.df <- sbrier(df, truth = observations, estimate = predictions)
  expect_equal(sbrier.df$.estimate, -0.2287708, tolerance = 0.001)
})
