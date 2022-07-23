
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calibratr

<!-- badges: start -->
<!-- badges: end -->

The goal of calibratr is to provide additional performance/calibration
metrics and features for use within the {yardstick} package in R. The
included functions enable evaluation of model performance from
predictive models developed using the {tidymodels} framework.

## Installation

You can install the development version of calibratr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mglev1n/calibratr")
```

## Examples

Calculate brier score using the `two_class_example` dataset from
`{yardstick}`:

``` r
library(calibratr)
library(yardstick)
#> For binary classification, the first factor level is assumed to be the event.
#> Use the argument `event_level = "second"` to alter this as needed.

yardstick::two_class_example %>%
  brier(truth = truth, estimate = Class1)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 brier   binary         0.106
```

Calculate scaled brier score using the `two_class_example` dataset from
{yardstick}:

``` r
library(calibratr)
library(yardstick)

yardstick::two_class_example %>%
  sbrier(truth = truth, estimate = Class1)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 sbrier  binary         0.577
```

Calibration plot using the `two_class_example` dataset from {yardstick}:

``` r
library(calibratr)
library(yardstick)
library(ggplot2)

yardstick::two_class_example %>%
  calib_curve(truth = truth, estimate = Class1) %>%
  autoplot()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
