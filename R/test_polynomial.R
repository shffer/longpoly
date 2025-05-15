#' Test models with polynomials of differing orders to describe the relationship between linear rate of change and mean score in longitudinal data
#'
#' @param data output from `longpoly::get_slopes_and_mean()` (or any tibble with columns "id", "performance_slope", & "performance_mean" or "performance_bl")
#' @param performance_metric which measure of performance is being used? Must be either "mean" (in which case performance_mean column must be in data) or "baseline" (data must contain performance_bl). Default = `"mean"`
#' @param test_proportion the proportion of participants to allocate to the test set. default = `1/3`
#' @param idcol the column name corresponding to the ID variable in data. Default = `"id"`
#' @param max_order the maximum order for polynomial models to be tested. Polynomial models for 1:`max_order` will be tested. default = `6`
#'
#' @return
#' a list containing:
#' 1. `polynomial_results` — a tibble with columns recording the order of each polynomial tested with the corresponding proportion of variance explained (PVE) in test and train data, as well as the additional PVE in the test data for the increase in order for each
#' 2. `train_ids`  — a character vector of ids allocated to the train dataset in model development
#' 3. `test_ids`  — a character vector of ids allocated to the test dataset in model development
#' 4. `scree_plot`  — a visualisation of the additional PVE for each polynomial. It is recommended that the order of the model be selected as that to the left of the ‘elbow’ (where the improvement plateaus).
#' @export
#' @import magrittr
#' @import ggplot2
#' @examples
#'
#' test_results <- test_polynomial(data = longpoly_example_data,
#'                                 idcol = "id",
#'                                 performance_metric = "mean",
#'                                 test_proportion = 1/3,
#'                                 max_order = 6)
#'
#' # View proportion of variance explained in the train and test data for each polynomial tested
#' test_results$polynomial_results
#'
#' # The IDs of the participants allocated to the train and test sets are stored in the following vectors
#' test_results$train_ids |> head()
#' test_results$test_ids |> head()
#'
#'# View the scree plot showing the additional variance explained (test data) for each increase in polynomial order
#' test_results$scree_plot



test_polynomial <-
  function(data,
           performance_metric = "mean",
           test_proportion = 1 / 3,
           idcol = "id",
           max_order = 6) {

    # column names (get_slopes_and_performance() output)
    if (!performance_metric %in% c("mean", "baseline")) {
      stop("performance_metric must be either \"mean\" or \"baseline\"")
    }

    outcome <- "performance_slope"
    predictor <- ifelse(performance_metric == "mean", "performance_mean", "performance_bl")

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
