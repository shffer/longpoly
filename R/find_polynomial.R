#' Title
#'
#' @param data
#' @param test_proportion
#' @param idcol
#' @param max_order
#' @param x
#'
#' @return
#' @export
#'
#' @examples
find_polynomial <- function(data, test_proportion, idcol, max_order, x) {


  # Column names
  outcome <- "performance_slope"
  predictor <- "performance_mean"

  # Split data into train and test
  unique_ids <- unique(data[[idcol]])
  sample_size <- round(length(unique_ids) * test_proportion)
  train_ids <- sample(unique_ids, size = sample_size, replace = FALSE)

  train <- data %>% filter(.data[[idcol]] %in% train_ids)
  test <- data %>% filter(!.data[[idcol]] %in% train_ids)
  test_ids <- test[[idcol]]

  # Function to compute proportion of variance explained
  compute_performance <- function(model, data, outcome) {
    pred <- predict(model, newdata = data)
    actual <- data[[outcome]]

    SS.total <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
    SS.regression <- sum((pred - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)

    result <- SS.regression / SS.total  # Fraction of variability explained
    return(result * 100)  # Convert to percentage
  }

  # Store performance results
  polynomial_results <- tibble(order = integer(), pve_in_train_data = numeric(), pve_in_test_data = numeric())
  models <- list()  # Store models for later retrieval

  for (order in 1:max_order) {
    formula <- as.formula(paste(outcome, "~ poly(", predictor, ",", order, ", raw=TRUE)"))
    model <- lm(formula, data = train)

    models[[as.character(order)]] <- model

    polynomial_results <- polynomial_results %>%
      add_row(
        order = order,
        pve_in_train_data = round(summary(model)$adj.r.squared * 100, 1),  # Convert to percentage and round
        pve_in_test_data = round(compute_performance(model, test, outcome), 1)  # Convert to percentage and round
      )
  }

  # Identify the best polynomial model
  max_pve_test <- max(polynomial_results$pve_in_test_data, na.rm = TRUE)
  threshold <- round(max_pve_test - (x * 100), 1)  # Convert x to percentage and round

  best_model_info <- polynomial_results %>%
    arrange(order) %>%  # Ensure sorting by order before filtering
    filter(pve_in_test_data >= threshold) %>%
    slice(1)  # Select the lowest order that meets the condition

  selected_order <- best_model_info$order

  # Retrieve the selected model
  selected_model <- models[[as.character(selected_order)]]

  # Run the selected model on the full dataset
  final_formula <- as.formula(paste(outcome, "~ poly(", predictor, ",", selected_order, ", raw=TRUE)"))
  final_model <- lm(final_formula, data = data)

  # Extract coefficients from the final model
  final_coefficients <- coef(final_model)

  # Return results
  return(list(
    polynomial_results = polynomial_results,
    selected_order = selected_order,
    selected_model = selected_model,
    selected_formula = final_formula,
    final_model_coefficients = final_coefficients,
    train_ids = train_ids,
    train_data = train,
    test_ids = test_ids,
    test_data = test
  ))
}
