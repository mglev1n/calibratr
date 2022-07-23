#' Calibration curve
#'
#' `calib_curve()` constructs bins of predicted probabilities for a binary classifier and returns a
#' tibble.
#'
#'  There is a [ggplot2::autoplot()]
#'  method for quickly visualizing the calibration plots. This currently works for
#'  binary outputs, and also works with grouped data (i.e. from
#'  resamples). This function is inspired by the `calib_plot` function from the `{gmish}` package, available at <https://github.com/gweissman/gmish>
#'
#' @family curve metrics
#'
#' @return
#' A tibble with class `calib_df` or `calib_grouped_df` having
#' columns `.bin`, `.truth`, `.estimate`, `.ci_lo`, and `.ci_high`.
#'
#' @author Michael Levin
#'
#' @family curve metrics
#' @templateVar metric_fn calib_curve
#' @template event_first
#'

#' @param data A data.frame containing the columns specified by `truth` and `...`
#' @param truth The unquoted column name corresponding to the `truth` column.
#' @param na_rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param event_level A single string. Either "first" or "second" to specify which level of truth to consider as the "event". This argument is only applicable when estimator = "binary".
#' @param case_weights (not currently used)
#' @param bins A single `numeric` value defining the number of probability bins to consider. Defaults to 10.
#' @param ... A set of unquoted column names or one or more dplyr selector functions to choose which variables contain the class probabilities. If truth is binary, only 1 column should be selected. Otherwise, there should be as many columns as factor levels of truth.

#' @import ggplot2
#' @import yardstick
#' @importFrom patchwork plot_layout
#' @importFrom Hmisc binconf
#'


#' @export
#'
calib_curve <- function(data, ...) {
  UseMethod("calib_curve")
}

#' @export
#' @rdname calib_curve
calib_curve.data.frame <- function(data,
                                truth,
                                ...,
                                bins = 10,
                                na_rm = TRUE,
                                event_level = yardstick:::yardstick_event_level(),
                                case_weights = NULL) {
  estimate <- dots_to_estimate(data, !!! enquos(...))

  result <- metric_summarizer(
    metric_nm = "calib_curve",
    metric_fn = calib_curve_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!estimate,
    na_rm = na_rm,
    event_level = event_level,
    metric_fn_options = list(bins = bins),
    case_weights = !!enquo(case_weights)
  )

  yardstick:::curve_finalize(result, data, "calib_df", "grouped_calib_df")
}

# Undecided of whether to export this or not
calib_curve_vec <- function(truth,
                         estimate,
                         na_rm = TRUE,
                         event_level = yardstick:::yardstick_event_level(),
                         case_weights = NULL,
                         bins = 10,
                         ...) {
  estimator <- finalize_estimator(truth, metric_class = "calib_curve")

  # `estimate` here is a matrix of class prob columns
  calib_curve_impl <- function(truth,
                            estimate,
                            ...,
                            case_weights = NULL,
                            bins = 10) {
    check_dots_empty()

    calib_curve_estimator_impl(
      truth = truth,
      estimate = estimate,
      estimator = estimator,
      event_level = event_level,
      case_weights = case_weights,
      bins = bins
    )
  }

  metric_vec_template(
    metric_impl = calib_curve_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    estimator = estimator,
    case_weights = case_weights,
    cls = c("factor", "numeric"),
    bins = bins
  )
}

calib_curve_estimator_impl <- function(truth,
                                    estimate,
                                    estimator,
                                    event_level,
                                    case_weights,
                                    bins) {
  if (yardstick:::is_binary(estimator)) {
    calib_curve_binary(truth, estimate, event_level, case_weights, bins)
  } else {
    # multiclass not yet implemented
    # calib_curve_multiclass(truth, estimate, case_weights)
  }
}

calib_curve_binary <- function(truth,
                            estimate,
                            event_level,
                            case_weights,
                            bins) {

    if (!rlang::is_bare_numeric(bins, n = 1L)) {
      abort("`bins` must be a single numeric value.")
    }
    if (!(bins >= 0)) {
      abort("`bins` must be a positive value.")
    }

    truth <- unclass(truth)

    # Convert to `1 == event`, `0 == non-event`
    if (yardstick:::is_event_first(event_level)) {
      truth <- as.integer(truth == 1L)
    } else {
      truth <- as.integer(truth == 2L)
    }

  df <- tibble(truth = truth, estimate = estimate)
  df %>%
    mutate(.bin = ntile(estimate, bins)) %>%
    group_by(.bin) %>%
    summarize(.estimate = mean(estimate),
              .truth = mean(truth),
              .ci_lo = binconf(sum(truth), n())[2],
              .ci_hi = binconf(sum(truth), n())[3])

}



# Dynamically exported
autoplot.calib_df <- function(object, ...){
  `%+%` <- ggplot2::`%+%`

  # Base chart
  calib_chart <- ggplot2::ggplot(data = object)

  # Add in group interactions if required
  if (inherits(object, "grouped_calib_df")) {

    grps <- dplyr::groups(object)

    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")

    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    calib_chart <- calib_chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {

    interact_expr <- list()

  }

  # splice in the group interactions, or do nothing
  aes_spliced <- ggplot2::aes(
    x = .estimate,
    y = .truth,
    ymin = .ci_lo,
    ymax = .ci_hi,
    !!! interact_expr
  )

  # build the graph
  calib <- calib_chart %+%
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "lightgray", linetype = "dotted") %+%
    ggplot2::geom_pointrange(mapping = aes_spliced) %+%
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) %+%
    ggplot2::coord_equal(ratio = 1) %+%
    ggplot2::theme_bw()

  # # If we have .level, that means this was multiclass
  # # and we want to show 1 vs all graphs
  # if (".level" %in% colnames(object)) {
  #   calib_chart <- calib_chart %+%
  #     ggplot2::facet_wrap(~.level)
  # }

  calib
}

