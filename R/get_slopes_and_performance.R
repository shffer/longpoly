#' Get individual linear slope and performance level (mean or baseline) from longitudinal data
#'
#' @param data a longitudinal data set
#' @param id_col the column name corresponding to the ID variable in data. Default = `"id"`
#' @param outcome_col the column name corresponding to the outcome variable in data. Default = `"outcome"`
#' @param time_col the column name corresponding to the time variable in data. Default = `"time"`
#' @param performance_metric which measure of performance should be used? Must be either "mean" or "baseline". Default = `"mean"`
#'
#' @return a `tibble` with columns for `id`, `performance_slope`, and one of either `performance_mean` or `performance_bl` depending on selection. This is what is used as inputs for subsequent `test_polynomial()` and `implement_polynomial()` function calls (and can therefore be directly piped in without storing intermediate output)
#' @export
#' @import magrittr
#' @import dplyr
#' @examples
#' # Create dummy data
#' n_participants = 5
#' n_timepoints = 3
#'
#' set.seed(2222)
#'
#' dummy <- data.frame(
#'   id = rep(1:n_participants, each = n_timepoints),
#'   timepoint = rep(1:n_timepoints, times = n_participants),
#'   performance = rnorm(n_participants * n_timepoints, mean = 0, sd = 1)
#' )
#'
#' # Extract slopes and mean from dummy data
#' get_slopes_and_performance(
#' data = dummy,
#' id_col = "id",
#' time_col = "timepoint",
#' outcome_col = "performance",
#' performance_metric = "mean"
#' )


get_slopes_and_performance <-
  function(data,
           id_col = "id",
           outcome_col = "outcome",
           time_col = "time",
           performance_metric = "mean") {

    if (!performance_metric %in% c("mean", "baseline")) {
      stop("performance_metric must be either \"mean\" or \"baseline\"")
    }

    summary_data <-
      data %>%
      group_by(across(all_of(id_col))) %>%
      summarise(
        performance_slope = coef(lm(
          reformulate(time_col, outcome_col), data = pick(everything())
        ))[2],
        performance_mean = mean(.data[[outcome_col]], na.rm = TRUE),
        .groups = "drop"
      )

    if (performance_metric == "mean") {
      return(summary_data)
    }

    if (performance_metric == "baseline") {
      merge(
        summary_data %>% select(-performance_mean),
        data %>%
          group_by(across(all_of(id_col))) %>%
          slice_min(.data[[time_col]]) %>%
          ungroup %>%
          select(all_of(c(id_col, outcome_col))) %>%
          rename(performance_bl = outcome_col),
        by = id_col
      )
    }

  }
