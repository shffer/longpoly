
<!-- README.md is generated from README.Rmd. Please edit that file -->

# longpoly <a href="https://github.com/shffer/longpoly"><img src="man/figures/longpoly_logo.png" align="right" height="155"/></a>

<!-- badges: start -->
<!-- badges: end -->

Longpoly provides a suite of tools to analyse longitudinal data. These
are intended to be applied in instances where expected rates of change
may vary across performance levels. The motivating use-case for longpoly
was to investigate how average rates of cognitive decline depend on mean
performance, but its utility extends to longitudinal data for other
outcomes where similar relationships may be observed.

This approach provides a novel “performance-adjusted” measure of rates
of change. Linear slopes are fit over an individual’s longitudinal
observations, and their mean score is calculated. Next, a polynomial is
fit at the group level to predict rates of change as a function of mean
score. A polynomial here allows for this relationship to be non-linear,
and longpoly provides tools to select the polynomial order. **Finally,
the residuals from this model are extracted as a measure of the extent
to which an individual’s rate of change is faster or slower *than is
typical for their mean level of performance*.**

In the case of cognitive decline, this approach also offers a
data-driven method to identify floor effects. Floor effects occur when
cognitive assessments lack sensitivity to provide accurate measurements
of performance in lower ranges. For longitudinal data, this means that
cognitive decline in this range also cannot be reliably quantified.
After fitting a polynomial model, longpoly can solve for the local
minimum in the lower range of mean performance to identify the point
where impaired performance becomes associated with slowed decline. While
this is interpreted to reflect the range of performance where floor
effects occur, it may alternatively reflect a true plateau in decline
that occurs with advanced impairment. However, identifying this effect
is also of interest as it is difficult to analyse rates of decline in
participants in a plateau alongside those elsewhere on the spectrum of
performance due to their fundamental differences. Longpoly can be used
to filter out individuals with performance in the range for floor
effects that it identifies.

## Installation

Longpoly can be installed from
[GitHub](https://github.com/shffer/longpoly) as follows.

``` r
# install.packages("devtools")
devtools::install_github("shffer/longpoly")
```

## Example

### 1. Get Slopes and Mean

The first step in the workflow is to submit a longitudinal data set to
`get_slopes_and_mean()`. This function returns a tibble with “id”,
“performance_slope”, and “performance_mean” columns. For the purposes of
this example, I will create a dummy dataset of five participants, each
with three timepoints.

``` r
n_participants = 5
n_timepoints = 3

set.seed(2222)

dummy <- data.frame(
  id = rep(1:n_participants, each = n_timepoints),
  timepoint = rep(1:n_timepoints, times = n_participants),
  performance = rnorm(n_participants * n_timepoints, mean = 0, sd = 1)
)

dummy |> head(n = 6)
#>   id timepoint performance
#> 1  1         1  -0.3380621
#> 2  1         2   0.9391643
#> 3  1         3   1.7377190
#> 4  2         1   0.6963261
#> 5  2         2   0.4622959
#> 6  2         3  -0.3150868
```

This can now be used with `get_slopes_and_mean()`. Load longpoly

``` r
library(longpoly)
```

Apply the function (specifying the column names)

``` r
sm <- get_slopes_and_mean(
  data = dummy,
  id_col = "id",
  time_col = "timepoint",
  outcome_col = "performance"
)

sm
#> # A tibble: 5 × 3
#>      id performance_slope performance_mean
#>   <int>             <dbl>            <dbl>
#> 1     1             1.04             0.780
#> 2     2            -0.506            0.281
#> 3     3             0.892            1.10 
#> 4     4            -1.06             0.827
#> 5     5             1.07            -0.853
```

#### A Note About Simulated Data

The dummy data used for `get_slopes_and_mean()` did not simulate any
relationship between mean and slope values and is therefore of limited
use in illustrating the utility of the other functions in longpoly.
Rather than using this output, the remainder of this example workflow
makes use of the `longpoly_example_data` example cognitive data set that
is shipped with longpoly. In creating this data, 1000 mean values
($\bar{X}$) were initially sampled from $N(0, 1)$ (cognitively
unimpaired participants) and another 1000 mean values were then sampled
from $N(-1.5, 0.75)$ (participants with cognitive impairment). The data
were combined and slopes ($Y$) were assigned conditionally as follows:

- $Y \sim N(-0.10, 0.2)$  if  $-0.5 \le \bar{X}$
- $Y \sim N(-0.30, 0.2)$  if  $-1.0 \le \bar{X} < -0.5$
- $Y \sim N(-0.45, 0.2)$  if  $-1.5 \le \bar{X} < -1$
- $Y \sim N(-0.25, 0.2)$  if  $-2.0 \le \bar{X} < -1.5$
- $Y \sim N(-0.15, 0.2)$  if  $\bar{X} < -2$

Given this, a non-linear relationship is expected where minimal decline
is observed when mean performance is above -0.5. For those with lower
mean performance, faster decline is expected until mean performance
reaches -1.5 at which point decline slows again. Floor effects are
therefore simulated to occur when $\bar{X} = -1.5$.

``` r
example_data <- longpoly::longpoly_example_data

example_data |> head(n = 10)
#> # A tibble: 10 × 3
#>       id performance_mean performance_slope
#>    <int>            <dbl>             <dbl>
#>  1     1            0.235           0.127  
#>  2     2           -0.331           0.0715 
#>  3     3           -0.312          -0.215  
#>  4     4           -2.30            0.126  
#>  5     5           -0.171          -0.215  
#>  6     6            0.140          -0.219  
#>  7     7           -1.50           -0.735  
#>  8     8           -1.01           -0.444  
#>  9     9           -0.948          -0.372  
#> 10    10           -0.494           0.00156
```

### 2. Test Polynomials

The tibble output from `get_slopes_and_mean()` can now be used for
testing models that describe the relationship between slope and mean
values using `test_polynomial()`. This function assigns participants in
to train and test data sets and fits polynomials up to a maximum order
specified by the user. It returns a list containing:

1.  ‘polynomial_results’ a tibble with columns recording the order of
    each polynomial tested with the corresponding PVE in test and train
    data, as well as the additional PVE in the test data for the
    increase in order for each

2.  ‘train_ids’ a character vector of ids allocated to the train dataset
    in model development

3.  ‘test_ids’ a character vector of ids allocated to the test dataset
    in model development

4.  ‘scree_plot’ visualising the additional proportion of variance
    exaplained (PVE) in the test data for higher order polynomial
    models. This is calculated as follows:

$$
\begin{aligned}
SS_{\text{residual}} &= \sum (x_i - \hat{x}_i)^2 \\
SS_{\text{regression}} &= \sum (\hat{x}_i - \bar{x})^2 \\
SS_{\text{total}} &= SS_{\text{regression}} + SS_{\text{residual}} \\
\\
PVE_{\text{test}} &= \frac{SS_{\text{regression}}}{SS_{\text{total}}}
\end{aligned}
$$

------------------------------------------------------------------------

**Where:**

- $x_i$ = Calculated slope for the $i^{\text{th}}$ individual test set
- $\hat{x}_i$ = Predicted slope for the $i^{\text{th}}$ individual in
  the test set
- $\bar{x}$ = Average of the slopes computed in the test set

It is recommended that the order of the model be select as that to the
left of the ‘elbow’ (where the improvement plateaus). For example, this
would be three in the below (output from this function)

``` r
library(longpoly)
## basic example code
```

<img src="man/figures/README-pressure-1.png" width="100%" />
