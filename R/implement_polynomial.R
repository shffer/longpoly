#' Fit a polynomial of a specified order to describe the relationship between linear rate of change and mean score in longitudinal data
#'
#' @param data output from `longpoly::get_slopes_and_mean()` (or any tibble with columns "id", "performance_slope", & "performance_mean" or "performance_bl")
#' @param performance_metric which measure of performance is being used? Must be either "mean" (in which case performance_mean column must be in data) or "baseline" (data must contain performance_bl). Default = `"mean"`
#' @param order the order of the polynomial model (recommended to be selected on the scree plot output from `longpoly::test_polynomial()`). default = `3`
#' @param floor_effects test for floor effects? These are identified by finding the mean value of the performance variable that is associated with the fastest rate of decline in the impaired range (specified by floor_range()). default = `FALSE`
#' @param floor_range (required if `floor_effects = TRUE`). the 'impaired' range over which to search for the value of "performance_mean" that returns the minimum value for "performance_slope". expects a vector of length two with the first value providing the lower bound of the range and the second value the upper.
#'
#' @return
#' a list containing:
#' 1. `model_formula` — the formula of the final model
#' 2. `final_data` — a tibble with columns for "id", "performance_slope", either "performance_mean" or "performance_bl" (depending on `performance_metric`), "predicted_slope", and "residual." If `floor_effects = TRUE`, an additional column "floor_effects" is appended with either keep/remove reflecting whether the record should be removed based on the identified performance_mean threshold for floor effects
#' 3. `threshold` — if `floor_effects = TRUE`, this records the threshold used for floor effect classifications
#'
#' @export
#' @import magrittr
#' @examples
#'
#' poly_out <-
#' implement_polynomial(
#'   data = longpoly_example_data,
#'   performance_metric = "mean",
#'   order = 3,
#'   floor_effects = TRUE,
#'   floor_range = c(min(longpoly_example_data$performance_mean), 0)
#' )
#'
#' # View the equation of the final model
#' poly_out$model_formula
#'
#' # View the final data
#' poly_out$final_data |> head()
#'
#' # View the selected threshold
#'  poly_out$threshold
#'
#' # Other example uses:
#'
#' # implement_polynomial(example_slope_mean_data,
#' #                      floor_effects = FALSE)
#' #
#' # # if floor_effects = TRUE,
#' # it may be sensible to use the minumum performance_mean value as the lower bound of the range,
#' # e.g.:
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
#' # # if floor_effects = FALSE,
#' # it is straightforward to pipe in the output from longpoly::get_slopes_and_mean(),
#' # e.g.:
#' # get_slopes_and_mean(example_data,
#' #                    id_col = "id",
#' #                    outcome_col = "memory_test",
#' #                    time_col = "time") %>%
#' #   implement_polynomial(., floor_effects = FALSE)
#'

implement_polynomial <-
  function(data,
           performance_metric = "mean",
           order = 3,
           floor_effects = FALSE,
           floor_range = NULL) {

    # column names (get_slopes_and_performance() output)
    if (!performance_metric %in% c("mean", "baseline")) {
      stop("performance_metric must be either \"mean\" or \"baseline\"")
    }

    outcome <- "performance_slope"
    predictor <- ifelse(performance_metric == "mean", "performance_mean", "performance_bl")

    formula <-
      as.formula(paste(outcome, "~ poly(", predictor, ",", order, ", raw=TRUE)"))
    model <- lm(formula, data = data)

    data$predicted_slope <- predict(model, newdata = data)
    data$residual <- data[[outcome]] - data$predicted_slope

    get_model_equation_text <- function(model, response = "y", predictor_symbol = "x") {
        coefs <- coef(model)
        degree <- length(coefs) - 1

        terms <- c()
        for (i in 0:degree) {
          coef_val <- round(coefs[i + 1], 3)

          # Format each term
          term <- if (i == 0) {
            sprintf("%.3f", coef_val)
          } else if (i == 1) {
            sprintf("%+.3f%s", coef_val, predictor_symbol)
          } else {
            sprintf("%+.3f%s^%d", coef_val, predictor_symbol, i)
          }

          terms <- c(terms, term)
        }

        equation <- paste(response, "=", paste(terms, collapse = " "))
        return(equation)
      }


    equation <- get_model_equation_text(model = model)

    threshold <- NA
    if (floor_effects) {
      if (is.null(floor_range) || length(floor_range) != 2) {
        stop("floor_range must be a numeric vector of length 2 when floor_effects is TRUE.")
      }

      coefs <- coef(model)
      deriv_coefs <- coefs[2:(order + 1)] * (1:order)
      critical_points <- polyroot(deriv_coefs)
      real_critical_points <-
        Re(critical_points[abs(Im(critical_points)) < 1e-10])

      # compute second derivative
      second_deriv_coefs <-
        deriv_coefs[2:length(deriv_coefs)] * (2:length(deriv_coefs))

      # evaluate second derivative at critical points
      is_minimum <- sapply(real_critical_points, function(x) {
        sum(second_deriv_coefs * x ^ (0:(length(
          second_deriv_coefs
        ) - 1))) > 0
      })

      valid_critical_points <- real_critical_points[is_minimum &
                                                      real_critical_points >= min(floor_range) &
                                                      real_critical_points <= max(floor_range)]

      if (length(valid_critical_points) == 0) {
        message(
          "No valid minimum found within the specified floor_range. No floor effects applied."
        )
        data$floor_effects <- "keep"
      } else {
        threshold <- valid_critical_points[1]
        data <- data %>%
          mutate(floor_effects = ifelse(.data[[predictor]] < threshold, "remove", "keep"))
      }
    } else {
      data$floor_effects <- "keep"
    }

    data <- data %>%
      mutate(floor_effects = factor(floor_effects, levels = c("keep", "remove")))

    return(list(
      model_formula = equation,
      final_data = data,
      threshold = threshold
    ))
  }
