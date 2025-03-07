#' Test models with polynomials of differing orders to describe the relationship between linear rate of change and mean score in longitudinal data
#'
#' @param data Output from `longpoly::get_slopes_and_mean()` (or any tibble with columns "id", "performance_slope", & "performance_mean")
#' @param test_proportion The proportion of participants to allocate to the test set. Default = `2/3`
#' @param max_order The maximum order for polynomial models to be tested. Polynomial models for 1:`max_order` will be tested. Default = `6`
#'
#' @return
#' A list containing:
#' 1. `polynomial_results` A tibble with columns recording the order of each polynomial tested with the corresponding PVE in test and train data, as well as the additional PVE in the test data for the increase in order for each
#' 2. `train_ids` A character vector of ids allocated to the train dataset in model development
#' 3. `test_ids` A character vector of ids allocated to the test dataset in model development
#' 4. `scree_plot` A visualisation of the additional PVE for each polynomial. It is recommended that the order of the model be select as that to the left of the ‘elbow’ (where the improvement plateaus).
#' @export
#'
#' @examples
#' #example()

test_polynomial <-
  function(data,
           test_proportion = 2 / 3,
           max_order = 6) {
    library(ggplot2)
    set.seed(2244)
    idcol = "id"
    # column names as expected from get_slopes_and_mean() output
    outcome <- "performance_slope"
    predictor <- "performance_mean"

    # assign train and test ids
    unique_ids <- unique(data[[idcol]])
    sample_size <- round(length(unique_ids) * test_proportion)
    train_ids <-
      sample(unique_ids, size = sample_size, replace = FALSE)

    train <- data %>% filter(.data[[idcol]] %in% train_ids)
    test <- data %>% filter(!.data[[idcol]] %in% train_ids)
    test_ids <- test[[idcol]]

    # compute proportion of variance explained
    compute_performance <- function(model, data, outcome) {
      pred <- predict(model, newdata = data)
      actual <- data[[outcome]]

      SS.total <-
        sum((actual - mean(actual, na.rm = TRUE)) ^ 2, na.rm = TRUE)
      SS.regression <-
        sum((pred - mean(actual, na.rm = TRUE)) ^ 2, na.rm = TRUE)

      result <-
        SS.regression / SS.total
      return(result)
    }

    # store pve values
    polynomial_results <-
      tibble(
        order = integer(),
        pve_in_train_data = numeric(),
        pve_in_test_data = numeric(),
        additional_pve = numeric()
      )

    previous_pve <- 0
    for (order in 1:max_order) {
      formula <-
        as.formula(paste(outcome, "~ poly(", predictor, ",", order, ", raw=TRUE)"))
      model <- lm(formula, data = train)

      pve_test <- compute_performance(model, test, outcome)
      additional_pve <- pve_test - previous_pve
      previous_pve <- pve_test

      polynomial_results <- polynomial_results %>%
        add_row(
          order = order,
          pve_in_train_data = compute_performance(model, train, outcome),
          pve_in_test_data = pve_test,
          additional_pve = additional_pve
        )
    }

    # generate scree plot
    scree_plot <-
      ggplot(polynomial_results, aes(x = order, y = additional_pve)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(1, max_order, by = 1)) +
      labs(title = "Scree Plot of Variance Explained by Polynomial Order",
           x = "Polynomial Order",
           y = "Additional Variance Explained") +
      theme_minimal()

    # return results
    return(
      list(
        polynomial_results = polynomial_results,
        train_ids = as.character(train_ids),
        test_ids = as.character(test_ids),
        scree_plot = scree_plot
      )
    )
  }
