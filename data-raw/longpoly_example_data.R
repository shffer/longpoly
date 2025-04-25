## code to prepare `longpoly_example_data` dataset goes here

usethis::use_data(longpoly_example_data, overwrite = TRUE)

# SIMULATE DATA ----
set.seed(4444)

## Parameters ----
cu_participants <- 1000
dx_participants <- 1000
n_timepoints <- 3
timepoints <- c(0, 12, 24, 36, 48)

# Generate mean performance values
performance <- append(rnorm(cu_participants, mean = 0, sd = 1),
                      rnorm(dx_participants, mean = -1.5, sd = 0.75))

# Assign slopes as a function of mean performance
assign_slope <- function(mean_perf) {
  if (mean_perf >= -0.5) {
    return(rnorm(1, mean = -0.1, sd = 0.2))
  } else if (mean_perf >= -1) {
    return(rnorm(1, mean = -0.3, sd = 0.2))
  }else if (mean_perf >= -1.5) {
    return(rnorm(1, mean = -0.45, sd = 0.2))
  } else if (mean_perf >= -2) {
    return(rnorm(1, mean = -0.25, sd = 0.2))
  } else {
    return(rnorm(1, mean = -0.15, sd = 0.2))
  }
}

# Create simulated dataset
longpoly_example_data <-
  tibble(id = 1:length(performance),
         performance_mean = performance) |>
  mutate(performance_slope = sapply(performance_mean, assign_slope))

usethis::use_data(longpoly_example_data)
