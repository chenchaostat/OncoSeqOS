
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
```

## Quick start

### Hazard rate from median survival time

``` r
lambda_from_median(median = 12)
#> [1] 0.0578
```

### Median of the sum of two exponential distributions

Formula-based calculation:

``` r
median_sum_exp(
  median_pfs = 3,
  median_postpd = 27,
  method = "formula"
)
#>    method median_pfs median_postpd median_total
#> 1 formula          3            27      31.5814
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
#>       method median_pfs median_postpd median_total mean_total ci_95_low ci_95_high  n_sim
#> 1 simulation          3            27      31.6826    43.3668    3.3048   147.8796 100000
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
#>           hr  log_hr     se       z      p medSurvT medSurvC SurvRate12C SurvRate12T n_events n_censor censor_rate
#> arm=1 0.6989 -0.3583 0.1738 -2.0617 0.0392  26.8423  19.8256      0.6351      0.7815      137      145      0.5142
ana_final
#>           hr  log_hr     se       z     p medSurvT medSurvC SurvRate12C SurvRate12T n_events n_censor censor_rate
#> arm=1 0.5919 -0.5244 0.1505 -3.4845 5e-04  26.8423  18.8378      0.6351      0.7815      187       95      0.3369
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
#>   n_simu prop_ctl_no prop_ctl_subseq1 prop_ctl_subseq2 prop_trt_no prop_trt_subseq1 prop_trt_subseq2 mean_hr_final median_hr_final sd_hr_final
#> 1    100        0.75             0.15              0.1        0.95             0.05                0        0.4982          0.4893      0.0771
#> 2    100        0.65             0.15              0.2        0.95             0.05                0        0.5299          0.5258      0.0736
#> 3    100        0.55             0.15              0.3        0.95             0.05                0        0.5676          0.5781      0.0853
#> 4    100        0.45             0.15              0.4        0.95             0.05                0        0.6121          0.6097      0.1005
#> 5    100        0.35             0.15              0.5        0.95             0.05                0        0.6509          0.6529      0.0922
#> 6    100        0.25             0.15              0.6        0.95             0.05                0        0.6986          0.6966      0.1095
#>   mean_medSurvT_final mean_medSurvC_final mean_SurvRate12T_final mean_SurvRate12C_final prob_hr_lt  POS final_CondPOS interim_POS mean_p_final
#> 1             26.5739             12.4554                 0.7296                 0.5094       1.00 1.00          0.05        0.95       0.0008
#> 2             26.3390             13.2157                 0.7317                 0.5316       1.00 1.00          0.03        0.97       0.0009
#> 3             26.5797             14.5847                 0.7263                 0.5655       0.98 0.98          0.18        0.80       0.0033
#> 4             26.0976             15.5533                 0.7264                 0.5932       0.92 0.92          0.20        0.72       0.0204
#> 5             26.6452             16.9084                 0.7313                 0.6216       0.87 0.87          0.35        0.52       0.0266
#> 6             25.7549             17.6816                 0.7209                 0.6391       0.72 0.72          0.36        0.36       0.0775
#>   median_p_final mean_censor_interim mean_censor_final
#> 1     1.1313e-06              0.5140            0.3077
#> 2     0.0000e+00              0.5141            0.3074
#> 3     2.0000e-04              0.5141            0.3078
#> 4     6.0000e-04              0.5140            0.3071
#> 5     3.3000e-03              0.5139            0.3087
#> 6     1.2800e-02              0.5138            0.3072
```

## Visualization

``` r
plots <- plot_pos_summary(
  summary_data = res$summary,
  x = "prop_ctl_subseq2",
  therapy_name = "CD20/CD30",
  return = "list"
)

plots$probability
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
