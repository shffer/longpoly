#' Title
#'
#' @param data output from `longpoly::get_slopes_and_mean()` (or any tibble with columns "id", "performance_slope", & "performance_mean")
#' @param test_proportion the proportion of participants to allocate to the test set. default = `1/3`
#' @param idcol the column name corresponding to the ID variable in data. Default = `"id"`
#' @param max_order the maximum order for polynomial models to be tested. Polynomial models for 1:`max_order` will be tested. default = `6`
#' @param x Polynomials are selected after finding the maximum proportion of variance explained (PVE) in test data across all models, and then finding the most parsimonious model with a PVE that is at least max(PVE) - x
#'
#' @return a list containing:
#' 1. 'polynomial_results' - a tibble with columns for the order of the model tested and the corresponding pve in the train and test data
#' 2. 'selected_order' - the order of the selected model (based on the max(PVE) - x criteria)
#' 3. 'selected_model_coefficients' - coefficients of the selected model (based on the max(PVE) - x criteria)
#' 4. 'train_ids' — a character vector of ids allocated to the train dataset in model development
#' 5. 'test_ids' — a character vector of ids allocated to the test dataset in model development
#' @export
#'
#' @examples
#' # example
find_polynomial <- function(data, test_proportion = 1/3, idcol = "id", max_order = 6, x) {


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
    selected_model_coefficients = final_coefficients,
    train_ids = train_ids,
    test_ids = test_ids
  ))
}
