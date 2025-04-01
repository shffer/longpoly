#' Get individual linear slope and mean values from longitudinal data
#'
#' @param data a longitudinal dataset
#' @param id_col the column name corresponding to the ID variable in data. Default = `"id"`
#' @param outcome_col the column name corresponding to the outcome variable in data. Default = `"outcome"`
#' @param time_col the column name corresponding to the time variable in data. Default = `"time"`
#'
#' @return a `tibble` with `id`, `performance_slope`, and `performance_mean` columns. This is what is used as inputs for subsequent `test_polynomial()` and `implement_polynomial()` function calls (and can therefore be directly piped in without storing intermediate output)
#' @export
#' @import magrittr
#' @import dplyr
#' @examples
#'
#' # get_slopes_and_mean(example_data, id_col = "id", outcome_col = "memory_test", time_col = "time")
#'

get_slopes_and_mean <-
  function(data,
           id_col = "id",
           outcome_col = "outcome",
           time_col = "time") {
    data %>%
      group_by(across(all_of(id_col))) %>%
      summarise(
        performance_slope = coef(lm(
          reformulate(time_col, outcome_col), data = pick(everything())
        ))[2],
        performance_mean = mean(.data[[outcome_col]], na.rm = TRUE),
        .groups = "drop"
      )
  }
