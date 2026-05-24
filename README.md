
<!-- README.md is generated from README.Rmd. Please edit README.Rmd -->

# OncoSeqOS

<!-- badges: start -->

<!-- badges: end -->

## Disclaimer

This software is provided for research and educational purposes only. It
is not intended to provide medical advice, clinical recommendations, or
regulatory guidance. Users are responsible for independently validating
all results before use in any clinical, regulatory, or commercial
decision-making.

## Overview

`OncoSeqOS` provides R functions for simulating how the proportion of
patients receiving subsequent anti-cancer therapy after progressive
disease may affect overall survival outcomes in oncology trials.

The package supports:

- Deriving hazard rates from median survival times.
- Calculating the median of the sum of two exponential distributions.
- Simulating oncology trials with different subsequent therapy patterns.
- Performing interim and final survival analyses.
- Running grid-based simulation scenarios.
- Visualizing probability of success summaries.

## Installation

You can install the development version of `OncoSeqOS` from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("chenchaostat/OncoSeqOS")
```

Load the package:

``` r
library(OncoSeqOS)
set_api_url("API url")
```

## Quick start

### Hazard rate from median survival time

``` r
lambda_from_median(median = 12)
```

### Median of the sum of two exponential distributions

Formula-based calculation:

``` r
median_sum_exp(
  median_pfs = 3,
  median_postpd = 27,
  method = "formula"
)
```

Simulation-based calculation:

``` r
median_sum_exp(
  median_pfs = 3,
  median_postpd = 27,
  method = "simulation",
  n_sim = 100000,
  seed = 2024
)
```

## Simulate one oncology trial

``` r
trial <- simulate_one_trial(
  n_total = 282,
  median_pfs_ctl = 3,
  median_pfs_trt = 14.6,
  prop_ctl_no = 0.30,
  prop_ctl_subseq1 = 0.15,
  prop_ctl_subseq2 = 0.55,
  median_os_ctl_no = 9.5,
  median_postpd_ctl_subseq1 = 22,
  median_postpd_ctl_subseq2 = 15,
  prop_trt_no = 0.9,
  prop_trt_subseq1 = 0.05,
  prop_trt_subseq2 = 0.05,
  median_os_trt_no = 25,
  median_postpd_trt_subseq1 = 22,
  median_postpd_trt_subseq2 = 15,
  interim_events = 138,
  final_events = 197,
  target_censor_rate = 0.29,
  seed = 20260427
)
```

> Note: The simulated data are intended for research and educational
> demonstration only.

## Analyze interim and final data

``` r
ana_interim <- analyse_trial(
  df = trial$interim,
  time_var = "os_time_interim",
  status_var = "os_status_interim"
)

ana_final <- analyse_trial(
  df = trial$final,
  time_var = "os_time_final",
  status_var = "os_status_final"
)

ana_interim
ana_final
```

## Grid simulation

``` r
res <- run_grid_simulation(
  n_simu = 10000,
  n_total = 282,
  interim_events = 138,
  final_events = 197,
  alpha_interim = 0.0147,
  alpha_final = 0.04551,
  median_pfs_ctl = 3,
  median_pfs_trt = 14.6,
  median_os_ctl_no = 9.5,
  median_postpd_ctl_subseq1 = 27,
  median_postpd_ctl_subseq2 = 15,
  median_os_trt_no = 25,
  median_postpd_trt_subseq1 = 27,
  median_postpd_trt_subseq2 = NULL,
  prop_ctl_subseq1 = 0.15,
  prop_ctl_subseq2 = seq(0.10, 0.80, by = 0.05),
  prop_trt_subseq1 = 0.05,
  prop_trt_subseq2 = 0,
  target_censor_rate = 0.29,
  hr_thr = 0.75,
  seed = 202604
)

head(res$summary)
```

For README rendering, you may want to use a smaller number of
simulations:

``` r
res <- run_grid_simulation(
  n_simu = 100,
  n_total = 282,
  interim_events = 138,
  final_events = 197,
  alpha_interim = 0.0147,
  alpha_final = 0.04551,
  median_pfs_ctl = 3,
  median_pfs_trt = 14.6,
  median_os_ctl_no = 9.5,
  median_postpd_ctl_subseq1 = 27,
  median_postpd_ctl_subseq2 = 15,
  median_os_trt_no = 25,
  median_postpd_trt_subseq1 = 27,
  median_postpd_trt_subseq2 = NULL,
  prop_ctl_subseq1 = 0.15,
  prop_ctl_subseq2 = seq(0.10, 0.80, by = 0.10),
  prop_trt_subseq1 = 0.05,
  prop_trt_subseq2 = 0,
  target_censor_rate = 0.29,
  hr_thr = 0.75,
  seed = 202604
)

head(res$summary)
```

## Visualization

``` r
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(rlang)
  library(stringr)
})
plots <- plot_pos_summary(
  summary_data = res$summary,
  x = "prop_ctl_subseq2",
  therapy_name = "CD20/CD30",
  return = "list"
)
plots$probability
plots$survival_rate_12m
plots$median_survival
plots$mean_hr
```

## Main functions

| Function | Description |
|----|----|
| `lambda_from_median()` | Convert median survival time to an exponential hazard rate. |
| `median_sum_exp()` | Calculate the median of the sum of two exponential distributions. |
| `simulate_one_trial()` | Simulate one oncology trial. |
| `analyse_trial()` | Analyze interim or final trial data. |
| `run_grid_simulation()` | Run grid-based simulation scenarios. |
| `plot_pos_summary()` | Visualize probability of success summaries. |

## Citation

If you use `OncoSeqOS` in your work, please cite this repository or the
corresponding package version.

## License

Please see the `LICENSE` file for details.
