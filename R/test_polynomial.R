test_polynomial <-
  function(data,
           test_proportion = 2 / 3,
           idcol = "id",
           max_order = 6) {
    library(ggplot2)
    set.seed(2244)

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
