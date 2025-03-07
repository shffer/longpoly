get_slopes_and_mean <- function(data, id_col, outcome, time) {
  data %>%
    group_by(across(all_of(id_col))) %>%
    summarise(
      performance_slope = coef(lm(
        reformulate(time, outcome), data = pick(everything())
      ))[2],
      performance_mean = mean(.data[[outcome]], na.rm = TRUE),
      .groups = "drop"
    )
}
