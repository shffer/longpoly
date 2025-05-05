#' Plot selected models from `final_data` tibble output from the `longpoly::implement_polynomial()` function
#'
#' @param data the `final_data` tibble output from the `longpoly::implement_polynomial()`
#' @param order the order of the polynomial model (recommended to be selected on the scree plot output from `longpoly::test_polynomial()`). default = `3`
#' @param x_label a character vector to optionally change the x axis label. default = "Mean Performance"
#' @param y_label a character vector to optionally change the y axis label. default = "Slope"
#' @param line_width the width of the line reflecting the polynomial equation. default = `1.5`
#' @param point_size the size of the individual data points. default = `3.5`
#' @param whole_cohort_point_color optionally set the color of the points in the whole cohort plot
#' @param whole_cohort_line_color optionally set the color of the line in the whole cohort plot
#' @param whole_cohort_title optionally set the title of the whole cohort plot. default = `NULL`
#' @param whole_cohort_only should only the whole cohort plot be generated? if `FALSE`, plots showing the train/test performance in `long_poly::test_polynomial()` for the selected order are returned. default = `TRUE`
#' @param train_id a vector of containing the IDs of observations allocated to the training dataset (recommended to use `train_ids` output from `long_poly::test_polynomial()`)
#' @param test_id a vector of containing the IDs of observations allocated to the test dataset (recommended to use `test_ids` output from `long_poly::test_polynomial()`)
#' @param train_point_color optionally set the color of the points in the training data plot
#' @param train_line_color optionally set the color of the line in the training data plot
#' @param train_title optionally set the title of the the training data plot. default = `NULL`
#' @param test_point_color optionally set the color of the points in the test data plot
#' @param test_line_color optionally set the color of the line in the test data plot
#' @param test_title optionally set the title of the test data plot. default = `NULL`
#' @param show_equation show the equation of the model in the plots? default = `TRUE`
#' @param keep_remove produce an additional plot showing IDs removed due to floor effects? default = `FALSE`
#' @param threshold (required if `keep_remove = TRUE`) the threshold used to keep or remove records due to floor_effects (recommended to use `threshold` output from `long_poly::implement_polynomial()`)
#' @param threshold_linetype ggplot linetype arguments to control type of line when `keep_remove = TRUE`. set to "blank" to remove line
#' @param threshold_line_color optionally set color of threhold line when `keep_remove = TRUE`
#' @param x_offset increase or decrease the x axis plotting region. this value is subtracted and added from the min and max values of performance_mean, respectively, to specify the plotting region in relation to the observed data. default = 0.25
#' @param y_offset increase or decrease the y axis plotting region. this value is subtracted and added from the min and max values of performance_slope, respectively, to specify the plotting region in relation to the observed data. default = 0.35
#' @param title_text_size ggplot title text size argument. default = `14`
#' @param axis_text_size ggplot axis title text size argument. default = `12`
#' @param remove_point_color optionally set the point color for removed participants due to floor effects when `keep_remove = TRUE` (those kept will be plotted with color specified in `whole_cohort_point_color`)
#' @param annotate_floor_thresh include an annotation specifying the threshold for floor effects when `keep_remove = TRUE`
#' @param legend_position ggplot legend position argument to control the placement of a legend identifying the participants kept and removed when `keep_remove = TRUE`. default = "none"
#' @param legend_title if a legend position is set and `keep_remove = TRUE`, this controls the title of the legend. default = "Floor Effects"
#' @param legend_title_size if a legend position is set and `keep_remove = TRUE`, this controls the size of the legend title (ggplot argument). default = 10
#' @param legend_text_size if a legend position is set and `keep_remove = TRUE`, this controls the title of the legend text (ggplot argument). deafult = 10
#' @param floor_keep_label if a legend position is set and `keep_remove = TRUE`, this controls the label of records with scores above the identified floor effects threshold. default = "Keep"
#' @param floor_remove_label if a legend position is set and `keep_remove = TRUE`, this controls the label of records with scores below the identified floor effects threshold. default = "Remove"
#'
#' @return
#' a list of ggplot objects:
#' 1. `whole_cohort` — the selected polynomial applied to all records
#' 2. `train` — (if requested) the selected polynomial applied to train data
#' 2. `test` — (if requested) the selected polynomial applied to test data. note that the polynomial developed in the train data is applied to the test data
#' 3. `keep_remove`  — (if requested) the whole_cohort plot with additional visualisation for the removal of records due to floor effects
#' @export
#' @import magrittr
#' @import ggplot2
#' @importFrom ggpubr ggscatter
#' @examples
#'
#' # test_polynomial() (or find_polynomial()) occur earlier in the workflow and assign train and test IDs
#' test_results <- test_polynomial(data = longpoly_example_data, test_proportion = 1/3, max_order = 6)
#'
#'
#' # implement polynomial is typically run before plotting (for identifying threshold)
#' poly_out <-
#' implement_polynomial(
#'   data = longpoly_example_data,
#'   order = 3,
#'   floor_effects = TRUE,
#'   floor_range = c(min(longpoly_example_data$performance_mean), 0)
#' )
#'
#'   plot_polynomial(
#' data = poly_out$final_data,
#' order = 3,
#' whole_cohort_title = "Selected Polynomial in Whole Cohort",
#' whole_cohort_only = FALSE,
#' train_id = test_results$train_ids,
#' test_id = test_results$test_ids,
#' train_title = "Selected Polynomial in Train Cohort",
#' test_title = "Selected Polynomial in Test Cohort",
#' keep_remove = TRUE,
#' threshold = poly_out$threshold,
#' threshold_linetype = "dashed",
#' threshold_line_color = "#5f6a7a",
#' annotate_floor_thresh = TRUE,
#' legend_position = "bottom",
#' )



plot_polynomial <- function(data,
                            order = 3,
                            x_label = "Mean Performance",
                            y_label = "Slope",
                            line_width = 1.5,
                            point_size = 3.5,
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
                            keep_remove = FALSE,
                            threshold = NULL,
                            threshold_linetype = "dashed",
                            threshold_line_color = "#5f6a7a",
                            x_offset = 0.25,
                            y_offset = 0.35,
                            title_text_size = 14,
                            axis_text_size = 12,
                            remove_point_color = "grey",
                            annotate_floor_thresh = FALSE,
                            legend_position = "none",
                            legend_title = "Floor Effects",
                            legend_title_size = 10,
                            legend_text_size = 10,
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
      size = point_size,
      color = whole_cohort_point_color,
      title = whole_cohort_title
    ) +
    stat_function(fun = poly_fun,
                  color = whole_cohort_line_color,
                  linewidth = line_width) +
    xlab(x_label) +
    ylab(y_label) +
    ylim(min(data$performance_slope) - y_offset,
         max(data$performance_slope) + y_offset) +
    xlim(min(data$performance_mean) - x_offset,
         max(data$performance_mean) + x_offset) +
    theme(plot.title = element_text(hjust = 0.5, size = title_text_size),
          axis.title = element_text(size = axis_text_size))
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
        title = train_title,
        size = point_size
      ) +
      stat_function(fun = poly_fun_train,
                    color = train_line_color,
                    linewidth = line_width) +
      xlab(x_label) +
      ylab(y_label) +
      ylim(min(data$performance_slope) - y_offset,
           max(data$performance_slope) + y_offset) +
      xlim(min(data$performance_mean) - x_offset,
           max(data$performance_mean) + x_offset) +
      theme(plot.title = element_text(hjust = 0.5, size = title_text_size),
            axis.title = element_text(size = axis_text_size))

    # test plot (note this uses the model from the train data)
    test_plot <-
      ggscatter(
        test_data,
        x = "performance_mean",
        y = "performance_slope",
        color = test_point_color,
        title = test_title,
        size = point_size
      ) +
      stat_function(fun = poly_fun_train,
                    color = test_line_color,
                    linewidth = line_width) +
      xlab(x_label) +
      ylab(y_label) +
      ylim(min(data$performance_slope) - y_offset,
           max(data$performance_slope) + y_offset) +
      xlim(min(data$performance_mean) - x_offset,
           max(data$performance_mean) + x_offset) +
      theme(plot.title = element_text(hjust = 0.5, size = title_text_size),
            axis.title = element_text(size = axis_text_size))

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
      geom_point(aes(color = floor_effects), size = point_size) +
      geom_vline(xintercept = threshold,
                 linetype = threshold_linetype,
                 color = threshold_line_color) +
      scale_color_manual(
        values = c("keep" = whole_cohort_point_color, "remove" = remove_point_color),
        labels = c("keep" = floor_keep_label, "remove" = floor_remove_label)
      ) +
      stat_function(fun = poly_fun,
                    color = whole_cohort_line_color,
                    linewidth = line_width) +
      theme(legend.position = legend_position,
            legend.title = element_text(size = legend_title_size),
            legend.text = element_text(size = legend_text_size))

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
