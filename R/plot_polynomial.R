plot_polynomial <- function(data,
                             x_label = "Mean Performance",
                             y_label = "Slope",
                             order = 3,
                             point_size = 1.5,
                             whole_cohort_point_color  = "#325a9c",
                             whole_cohort_line_color = "#110036",
                             whole_cohort_title = NULL,
                             whole_cohort_only = TRUE,
                             train_id = NULL,
                             test_id = NULL,
                             train_point_color = "#430C33",
                             train_line_color = "#7B115B",
                             train_title = NULL,
                             test_point_color = "#113B19",
                             test_line_color = "#7B115B",
                             test_title = NULL,
                             show_equation = TRUE,
                             keep_remove = TRUE,
                             threshold = NULL,
                             threshold_linetype = "dashed",
                             threshold_line_color = "#5f6a7a",
                             x_offset = 0.25,
                             y_offset = 0.35,
                             remove_point_color = "grey",
                             annotate_floor_thresh = FALSE,
                             legend_position = "none",
                             legend_title = "Floor Effects",
                             floor_keep_label = "Keep",
                             floor_remove_label = "Remove") {
  # ensure 'floor_effects' is properly assigned as a factor in the tibble
  data <- data %>%
    mutate(floor_effects = factor(floor_effects, levels = unique(floor_effects)))


  # fit polynomial regression
  poly_fit <-
    lm(performance_slope ~ poly(performance_mean, order, raw = TRUE),
       data = data)
  coeffs <- coef(poly_fit)

  # define function
  poly_fun <- function(x) {
    sapply(x, function(x_i)
      sum(coeffs * x_i ^ (0:(
        length(coeffs) - 1
      ))))
  }

  # whole cohort plot
  whole_cohort <-
    ggscatter(
      data,
      x = "performance_mean",
      y = "performance_slope",
      color = whole_cohort_point_color,
      title = whole_cohort_title
    ) +
    stat_function(fun = poly_fun,
                  color = whole_cohort_line_color,
                  linewidth = point_size) +
    xlab(x_label) +
    ylab(y_label) +
    ylim(min(data$performance_slope) - y_offset,
         max(data$performance_slope) + y_offset) +
    xlim(min(data$performance_mean) - x_offset,
         max(data$performance_mean) + x_offset) +
    theme(plot.title = element_text(hjust = 0.5))
  # add equation if show_equation = TRUE
  if (show_equation) {
    whole_cohort <- whole_cohort +
      stat_regline_equation(
        aes(x = performance_mean, y = performance_slope),
        label.x.npc = 0,
        label.y.npc = 0,
        formula = y ~ poly(x, order, raw = TRUE),
        size = 3.5
      )
  }

  # train and test plots
  plots <- list(whole_cohort = whole_cohort)

  if (!whole_cohort_only) {
    if (is.null(train_id) | is.null(test_id)) {
      stop("train_id and test_id must be provided when whole_cohort_only = FALSE")
    }

    train_data <- filter(data, id %in% train_id)
    test_data <- filter(data, id %in% test_id)

    # generate function from train data
    train_coeffs <-
      coef(lm(
        performance_slope ~ poly(performance_mean, order, raw = TRUE),
        data = train_data
      ))
    poly_fun_train <- function(x) {
      sapply(x, function(xi)
        sum(train_coeffs * xi ^ (0:(
          length(train_coeffs) - 1
        ))))
    }

    # format equation as a string
    format_equation <- function(coeffs) {
      terms <- paste0(round(coeffs, 3), " * x^", seq_along(coeffs) - 1)
      equation <- paste(terms, collapse = " + ")
      paste("y =", equation)
    }

    train_equation <-
      format_equation(train_coeffs)

    # train Plot
    train_plot <-
      ggscatter(
        train_data,
        x = "performance_mean",
        y = "performance_slope",
        color = train_point_color,
        title = train_title
      ) +
      stat_function(fun = poly_fun_train,
                    color = train_line_color,
                    linewidth = point_size) +
      xlab(x_label) +
      ylab(y_label) +
      ylim(min(data$performance_slope) - y_offset,
           max(data$performance_slope) + y_offset) +
      xlim(min(data$performance_mean) - x_offset,
           max(data$performance_mean) + x_offset) +
      theme(plot.title = element_text(hjust = 0.5))

    # test plot (note this uses the model from the train data)
    test_plot <-
      ggscatter(
        test_data,
        x = "performance_mean",
        y = "performance_slope",
        color = test_point_color,
        title = test_title
      ) +
      stat_function(fun = poly_fun_train,
                    color = test_line_color,
                    linewidth = point_size) +
      xlab(x_label) +
      ylab(y_label) +
      ylim(min(data$performance_slope) - y_offset,
           max(data$performance_slope) + y_offset) +
      xlim(min(data$performance_mean) - x_offset,
           max(data$performance_mean) + x_offset) +
      theme(plot.title = element_text(hjust = 0.5))

    # position the equation equally in both plots
    if (show_equation) {
      equation_x <-
        min(data$performance_mean) + 0.05 * diff(range(data$performance_mean))
      equation_y <-
        min(data$performance_slope) + 0.05 * diff(range(data$performance_slope))

      train_plot <- train_plot +
        annotate(
          "text",
          x = min(data$performance_mean) - x_offset,
          y = min(data$performance_slope) - y_offset,
          label = train_equation,
          hjust = 0,
          size = 3.5
        )

      test_plot <- test_plot +
        annotate(
          "text",
          x = min(data$performance_mean) - x_offset,
          y = min(data$performance_slope) - y_offset,
          label = train_equation,
          hjust = 0,
          size = 3.5
        )
    }

    plots$train <- train_plot
    plots$test <- test_plot
  }


  # keep/remove plot
  if (keep_remove) {
    if (is.null(threshold)) {
      stop("threshold must be provided when keep_remove = TRUE")
    }

    keep_remove_plot <- whole_cohort +
      geom_point(aes(color = floor_effects), size = 2) +
      geom_vline(xintercept = threshold,
                 linetype = threshold_linetype,
                 color = threshold_line_color) +
      scale_color_manual(
        values = c("keep" = whole_cohort_point_color, "remove" = remove_point_color),
        labels = c("keep" = floor_keep_label, "remove" = floor_remove_label)
      ) +
      stat_function(fun = poly_fun,
                    color = whole_cohort_line_color,
                    linewidth = point_size) +
      theme(legend.position = legend_position)  #

    # add legend title if requested
    if (!is.null(legend_title)) {
      keep_remove_plot <- keep_remove_plot + labs(color = legend_title)
    }

    # add threshold annotation if requested
    if (annotate_floor_thresh) {
      keep_remove_plot <- keep_remove_plot +
        annotate(
          "text",
          x = threshold,
          y = max(data$performance_slope),
          label = paste0("x = ", round(threshold, 2)),
          hjust = -0.1,
          size = 4
        )
    }

    plots$keep_remove <- keep_remove_plot
  }
  return(plots)
}
