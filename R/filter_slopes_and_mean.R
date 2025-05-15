#' Filter slope and mean data to remove scarce data ranges before fitting polynomial
#'
#' @param data output from `longpoly::get_slopes_and_mean()` (or any tibble with columns "id", "performance_slope", & "performance_mean" or "performance_bl")
#' @param performance_metric which measure of performance is being used? Must be either "mean" (in which case performance_mean column must be in data) or "baseline" (data must contain performance_bl). Default = `"mean"`
#' @param window_size the difference added to /subtracted from the minuimum/maximum values to subset the data. The number of observations in this subset is counted
#' @param min_obs the minimum number of observations required in the subset defined by `window_size`. If the fewer observations exist, data in these ranges are filtered out
#' @param max_filter filter highest scores? default = `TRUE`
#' @param min_filter filter lowest scores? default = `TRUE`
#'
#' @return a dataset containing slope and mean values with sparse data removed (if requested and identified)
#' @export
#' @import magrittr
#'
#' @examples
#'
#' longpoly_example_data %>% nrow
#'
#' filtered_example_data <- filter_slopes_and_mean(
#' data = longpoly_example_data,
#' performance_metric = "mean",
#' window_size = 0.25,
#' min_obs = 20,
#' max_filter = TRUE,
#' min_filter = TRUE
#' )
#'
#' filtered_example_data %>% nrow

filter_slopes_and_mean <-
  function(data,
           performance_metric = "mean",
           window_size,
           min_obs,
           max_filter = TRUE,
           min_filter = TRUE) {
    if (window_size < 0) {
      stop("Error: window_size must be non-negative.")
    }

    if (!performance_metric %in% c("mean", "baseline")) {
      stop("performance_metric must be either \"mean\" or \"baseline\"")
    }


    if (performance_metric == "mean") {
      mean <- "performance_mean"

      max_cutoff <-  max(data[[mean]])
      min_cutoff <-  min(data[[mean]])

      if (max_filter) {
        n_obs_max <- data %>%
          filter(performance_mean > (max(data[[mean]]) - window_size)) %>% nrow

        max_cutoff <- ifelse(n_obs_max < min_obs,
                             max(data[[mean]]) - window_size,
                             max_cutoff)

        print(paste("number of observations in max window: ", n_obs_max))
        print(paste("user defined miniumum number of observations: ", min_obs))

        ifelse(
          n_obs_max < min_obs,
          print("data in max window removed"),
          print("data in max window not removed")
        )
      }

      if (min_filter) {
        n_obs_min <- data %>%
          filter(performance_mean < min(data[[mean]]) + window_size) %>% nrow

        min_cutoff <- ifelse(n_obs_min < min_obs,
                             min(data[[mean]]) + window_size,
                             min_cutoff)

        print(paste("number of observations in min window: ", n_obs_min))
        print(paste("user defined miniumum number of observations: ", min_obs))

        ifelse(
          n_obs_min < min_obs,
          print("data in min window removed"),
          print("data in min window not removed")
        )

      }

      filtered_data <-
        data %>%
        filter(performance_mean <= max_cutoff,
               performance_mean >= min_cutoff)

      return(filtered_data)

    }

    if (performance_metric == "baseline") {
      bl <- "performance_bl"

      max_cutoff <-  max(data[[bl]])
      min_cutoff <-  min(data[[bl]])

      if (max_filter) {
        n_obs_max <- data %>%
          filter(performance_bl > (max(data[[bl]]) - window_size)) %>% nrow

        max_cutoff <- ifelse(n_obs_max < min_obs,
                             max(data[[bl]]) - window_size,
                             max_cutoff)

        print(paste("number of observations in max window: ", n_obs_max))
        print(paste("user defined miniumum number of observations: ", min_obs))

        ifelse(
          n_obs_max < min_obs,
          print("data in max window removed"),
          print("data in max window not removed")
        )
      }

      if (min_filter) {
        n_obs_min <- data %>%
          filter(performance_bl < min(data[[bl]]) + window_size) %>% nrow

        min_cutoff <- ifelse(n_obs_min < min_obs,
                             min(data[[bl]]) + window_size,
                             min_cutoff)

        print(paste("number of observations in min window: ", n_obs_min))
        print(paste("user defined miniumum number of observations: ", min_obs))

        ifelse(
          n_obs_min < min_obs,
          print("data in min window removed"),
          print("data in min window not removed")
        )

      }

      filtered_data <-
        data %>%
        filter(performance_mean <= max_cutoff,
               performance_mean >= min_cutoff)

      return(filtered_data)

    }

  }
