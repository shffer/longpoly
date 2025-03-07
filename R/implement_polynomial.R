#' Fit a polynomial of a specified order to describe the relationship between linear rate of change and mean score in longitudinal data
#'
#' @param data output from `longpoly::get_slopes_and_mean()` (or any tibble with columns "id", "performance_slope", & "performance_mean")
#' @param order the order of the polynomial model (recommended to be selected on the scree plot output from `longpoly::test_polynomial()`). default = `3`
#' @param floor_effects test for floor effects? These are identified by finding the mean value of the performance variable that is associated with the fastest rate of decline in the impaired range (specified by floor_range()). default = `FALSE`
#' @param floor_range (required if `floor_effects = TRUE`). the 'impaired' range over which to search for the value of "performance_mean" that returns the minimum value for "performance_slope". expects a vector of length two with the first value providing the lower bound of the range and the second value the upper.
#'
#' @return
#' a list containing:
#' 1. `model_formula` — the formula of the final model
#' 2. `final_data` — a tibble with columns for "id", "performance_slope", "performance_mean", "predicted_slope", and "residual." If `floor_effects = TRUE`, an additional column "floor_effects" is appended with either keep/remove reflecting whether the record should be removed based on the identified performance_mean threshold for floor effects
#' 3. `threshold` — if `floor_effects = TRUE`, this records the threshold used for floor effect classifications
#'
#' @export
#'
#' @examples
#'
#' # implement_polynomial(example_slope_mean_data,
#' #                      floor_effects = FALSE)
#' #
#' # # if floor_effects = TRUE, it may be sensible to use the minumum performance_mean value as the lower bound of the range, e.g.:
#' # example_slope_mean_data <-
#' #   longpoly::get_slopes_and_mean(
#' #     example_data,
#' #     id_col = "id",
#' #     outcome_col = "memory_test",
#' #     time_col = "time"
#' #   ))
#' #
#' #
#' # implement_polynomial(
#' #   example_slope_mean_data,
#' #   floor_effects = TRUE,
#' #   floor_range = c(min(
#' #     example_slope_mean_data$performance_mean
#' #   ), 0)
#' # )
#' #
#' #
#' # # if floor_effects = FALSE, it is straightforward to pipe in the output from longpoly::get_slopes_and_mean(), e.g.:
#' # get_slopes_and_mean(example_data,
#' #                    id_col = "id",
#' #                    outcome_col = "memory_test",
#' #                    time_col = "time") %>%
#' #   implement_polynomial(., floor_effects = FALSE)
#'

implement_polynomial <-
  function(data,
           order = 3,
           floor_effects = FALSE,
           floor_range = NULL) {

    # column names as expected from get_slopes_and_mean() output
    outcome <- "performance_slope"
    predictor <- "performance_mean"

    # fit model
    formula <-
      as.formula(paste(outcome, "~ poly(", predictor, ",", order, ", raw=TRUE)"))
    model <- lm(formula, data = data)

    # generate predictions and residuals
    data$predicted_slope <- predict(model, newdata = data)
    data$residual <- data[[outcome]] - data$predicted_slope

    # handle floor effects if requested
    threshold <- NA
    if (floor_effects) {
      if (is.null(floor_range) || length(floor_range) != 2) {
        stop(
          "floor_range must be provided as a numeric vector of length 2 when floor_effects is TRUE."
        )
      }

      # subset data within the specified range
      floor_subset <- data %>%
        filter(between(.data[[predictor]], floor_range[1], floor_range[2]))

      if (nrow(floor_subset) == 0) {
        message("No values found in the specified floor_range. No floor effects applied.")
        data$floor_effects <- "keep"
      } else {
        # find the threshold value where the minimum slope occurs within the range
        min_slope_row <- floor_subset %>%
          filter(.data[[outcome]] == min(.data[[outcome]], na.rm = TRUE))

        if (nrow(min_slope_row) == 0) {
          message("No minimum found within the specified range. No floor effects applied.")
          data$floor_effects <- "keep"
        } else {
          threshold <- min_slope_row[[predictor]][1]
          data <- data %>%
            mutate(floor_effects = ifelse(.data[[predictor]] < threshold, "remove", "keep"))
        }
      }
    } else {
      data$floor_effects <- "keep"
    }

    data <- data %>%
      mutate(floor_effects = factor(floor_effects, levels = c("keep", "remove")))

    # return results
    return(list(
      model_formula = formula,
      final_data = data,
      threshold = threshold
    ))
  }
