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
