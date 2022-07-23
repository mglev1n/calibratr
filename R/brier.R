#' Brier score
#'
#' @description
#' `brier()` is a metric that computes the Brier score, given by: \deqn{ BS = \frac{1}{N} \sum (y - \hat y)^2}
#' `sbrier()` is a metric that computes the scaled Brier score (Brier Skill Score), given by: \deqn{ BS_{scaled} = 1 - \frac{BS}{BS_{max}}} where
#' \deqn{BS_{max} = \frac{1}{N} \sum (y - \bar y)^2}
#'
#' @details
#' Brier scores within the interval between 0 and 1, with scores closer to 0 indicating better model performance. The scaled Brier score (also known as Brier Skill Score) ranges from `-Inf` to 1, with scores closer to 1 indicating better model performance. Scores less than 0 represent models performing worse than a reference, and scores equal to 1 represent models equivalent to predicting the overall frequency of the class being predicted within the dataset being scored.
#'
#' @family class probability metrics
#'
#' @param data A data.frame containing the columns specified by `truth` and `...`
#' @param truth The unquoted column name corresponding to the `truth` column.
#' @param estimate If truth is binary, a numeric vector of class probabilities corresponding to the "relevant" class.
#' @param estimator "binary" is only relevant for the two class case
#' @param na_rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param event_level A single string. Either "first" or "second" to specify which level of truth to consider as the "event". This argument is only applicable when estimator = "binary".
#' @param ... not currently used
#'
#' @references
#' Brier, Glenn (1950). "VERIFICATION OF FORECASTS EXPRESSED IN TERMS OF PROBABILITY." _Monthly Weather Review_. Vol 78, Iss 1, pp 1-3
#'
#' @author Michael Levin
#'
#' @family prob metrics
#' @templateVar metric_fn brier
#' @template event_first
#'
#' @import yardstick rlang dplyr tidyr


# Yardstick implementation of brier score

#' @export

brier <- function(data, ...) {
  UseMethod("brier")
}


#' @export
#' @rdname brier
brier.data.frame <- function(data,
                             truth,
                             estimate,
                             estimator = NULL,
                             na_rm = TRUE,
                             event_level = "first",
                             ...) {
  metric_summarizer(
    metric_nm = "brier",
    metric_fn = brier_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

#' @export
#' @rdname brier
brier_vec <- function(truth,
                      estimate,
                      estimator = NULL,
                      na_rm = TRUE,
                      event_level = "first",
                      ...) {
  # calls finalize_estimator_internal() internally
  estimator <- finalize_estimator(truth, estimator, metric_class = "brier")

  brier_impl <- function(truth, estimate) {
    # Create
    if(identical(event_level, "first")) {
      mean((abs(2-as.numeric(truth)) - estimate) ^ 2)
    } else {
      mean((abs(1-as.numeric(truth)) - estimate) ^ 2)
    }

  }

  metric_vec_template(
    metric_impl = brier_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    estimator = estimator,
    ...
  )
}

brier <- yardstick::new_prob_metric(brier, direction = "minimize")

finalize_estimator_internal.brier <- function(metric_dispatcher, x, estimator) {
  validate_estimator(estimator, estimator_override = "binary")

  if(!is.null(estimator)) {
    return(estimator)
  }

  lvls <- levels(x)

  if(length(lvls) > 2) {
    stop("A multiclass `truth` input was provided, but only `binary` is supported.")
  }

  "binary"
}


# Yardstick implementation of scaled brier score
finalize_estimator_internal.sbrier <- function(metric_dispatcher, x, estimator) {
  validate_estimator(estimator, estimator_override = "binary")

  if(!is.null(estimator)) {
    return(estimator)
  }

  lvls <- levels(x)

  if(length(lvls) > 2) {
    stop("A multiclass `truth` input was provided, but only `binary` is supported.")
  }

  "binary"
}

#' @export
#' @rdname brier
sbrier_vec <- function(truth,
                       estimate,
                       estimator = NULL,
                       na_rm = TRUE,
                       event_level = "first",
                       ...) {
  # calls finalize_estimator_internal() internally
  estimator <- finalize_estimator(truth, estimator, metric_class = "brier")

  sbrier_impl <- function(truth, estimate) {
    # Create
    if(identical(event_level, "first")) {
      1 - mean((abs(2-as.numeric(truth)) - estimate) ^ 2) / (mean((rep(mean(abs(2-as.numeric(truth))), length(truth)) - abs(2-as.numeric(truth)))^2))
    } else {
      1 - mean((abs(1-as.numeric(truth)) - estimate) ^ 2) / (mean((rep(mean(abs(1-as.numeric(truth))), length(truth)) - abs(1-as.numeric(truth)))^2))
    }

  }

  metric_vec_template(
    metric_impl = sbrier_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = c("factor", "numeric"),
    estimator = estimator,
    ...
  )
}

#' @export
#' @rdname brier
sbrier <- function(data, ...) {
  UseMethod("sbrier")
}

sbrier <- yardstick::new_prob_metric(sbrier, direction = "maximize")

#' @export
#' @rdname brier
sbrier.data.frame <- function(data,
                              truth,
                              estimate,
                              estimator = NULL,
                              na_rm = TRUE,
                              event_level = "first",
                              ...) {
  metric_summarizer(
    metric_nm = "sbrier",
    metric_fn = sbrier_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}
