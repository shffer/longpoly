#' Get individual linear slope and mean values from longitudinal data
#'
#' @param data A longitudinal dataset
#' @param id_col The column name corresponding to the ID variable in data. Default = `"id"`
#' @param outcome_col The column name corresponding to the outcome variable in data. Default = `"outcome"`
#' @param time_col The column name corresponding to the time variable in data. Default = `"time"`
#'
#' @return A `tibble` with `id`, `performance_slope`, and `performance_mean` columns. This is what is used as inputs for subsequent `test_polynomial()` and `implement_polynomial()` function calls (and can therefore be directly piped in without storing intermediate output)
#' @export
#'
#' @examples
#' # create example data with simulated effects for both time and mean performance on longitudinal rate of change
#' set.seed(222)
#'n_participants <- 100
#'n_timepoints <- 5
#'timepoints <- c(12, 24, 36, 48, 60)
#'baseline_scores <- rnorm(n_participants, mean = 0, sd = 1)
#'assign_slope <- function(mean_perf) {
#'  if (mean_perf >= 0.75) {
#'    return(rnorm(1, mean = 0, sd = 0.1) * -1)
#'  } else if (mean_perf >= -1.5) {
#'    return(rnorm(1, mean = 0.4, sd = 0.15) * -1)
#'  } else if (mean_perf >= -2) {
#'    return(rnorm(1, mean = 0.2, sd = 0.15) * -1)
#'  } else {
#'    return(rnorm(1, mean = 0, sd = 0.1) * -1)
#'  }
#'}
#'example_data <- do.call(rbind, lapply(1:n_participants, function(id) {
#'  mean_perf <- baseline_scores[id]
#'  slope <- assign_slope(mean_perf)
#'  time <- rep(timepoints, each = 1)
#'  performance <- mean_perf + slope * (time / 12) + rnorm(n_timepoints, 0, 0.2)
#'  data.frame(id = id, time = time, performance = performance)
#'}))
#'
#'
#' get_slopes_and_mean(test, id_col = "id", outcome_col = "performance", time_col = "time")

get_slopes_and_mean <-
  function(data,
           id_col = "id",
           outcome_col = "outcome",
           time_col = "time") {
    data %>%
      group_by(across(all_of(id_col))) %>%
      summarise(
        performance_slope = coef(lm(
          reformulate(time, outcome), data = pick(everything())
        ))[2],
        performance_mean = mean(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      )
  }
