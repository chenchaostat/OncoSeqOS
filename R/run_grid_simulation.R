#' @title Run Grid Simulation Through API
#' @description
#' Calls server API to run grid simulation for oncology clinical trial analysis.
#'
#' @param n_simu Number of simulation runs.
#' @param n_total Total sample size.
#' @param interim_events Number of interim events.
#' @param final_events Number of final events.
#' @param alpha_interim Interim alpha.
#' @param alpha_final Final alpha.
#' @param median_pfs_ctl Median PFS in control arm.
#' @param median_pfs_trt Median PFS in treatment arm.
#' @param median_os_ctl_no Median OS in control arm without subsequent therapy.
#' @param median_postpd_ctl_subseq1 Median post-PD survival in control subseq1.
#' @param median_postpd_ctl_subseq2 Median post-PD survival in control subseq2.
#' @param median_os_trt_no Median OS in treatment arm without subsequent therapy.
#' @param median_postpd_trt_subseq1 Median post-PD survival in treatment subseq1.
#' @param median_postpd_trt_subseq2 Median post-PD survival in treatment subseq2.
#' @param prop_ctl_subseq1 Proportion of control subseq1.
#' @param prop_ctl_subseq2 Proportion vector of control subseq2.
#' @param prop_trt_subseq1 Proportion of treatment subseq1.
#' @param prop_trt_subseq2 Proportion of treatment subseq2.
#' @param hr_thr HR threshold.
#' @param enroll_duration Enrollment duration.
#' @param target_censor_rate Target censor rate.
#' @param seed Random seed.
#'
#' @returns A list containing summary and detail simulation results.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- run_grid_simulation(
#'   n_simu = 100,
#'   prop_ctl_subseq2 = seq(0.10, 0.80, by = 0.05)
#' )
#'
#' head(res$summary)
#' }






run_grid_simulation <- function(
    n_simu = 1000,
    n_total = 282,
    interim_events = 138,
    final_events = 197,
    alpha_interim = 0.0147,
    alpha_final = 0.04551,
    median_pfs_ctl = 3,
    median_pfs_trt = 14.6,
    median_os_ctl_no = 9.5,
    median_postpd_ctl_subseq1 = 22,
    median_postpd_ctl_subseq2 = 15,
    median_os_trt_no = 25,
    median_postpd_trt_subseq1 = 22,
    median_postpd_trt_subseq2 = 15,
    prop_ctl_subseq1 = 0.15,
    prop_ctl_subseq2 = seq(0.10, 0.80, by = 0.05),
    prop_trt_subseq1 = 0.05,
    prop_trt_subseq2 = 0.05,
    hr_thr = 0.8,
    enroll_duration = 11,
    target_censor_rate = 0.29,
    seed = 20260427
) {

  set.seed(seed)
  params <- list(
    prop_ctl_subseq1 = prop_ctl_subseq1,
    prop_ctl_subseq2 = prop_ctl_subseq2,
    prop_trt_subseq1 = prop_trt_subseq1,
    prop_trt_subseq2 = prop_trt_subseq2
  )
  varying_vars <- names(params)[sapply(params, length) > 1]
  fixed_vars <- names(params)[sapply(params, length) == 1]
  grid <- tibble(
    !!varying_vars[1] := params[[varying_vars[1]]]
  )
  for (v in fixed_vars) {
    grid[[v]] <- params[[v]]
  }
  grid <- grid %>%
    mutate(
      prop_ctl_no = 1 - prop_ctl_subseq1 - prop_ctl_subseq2,
      prop_trt_no = 1 - prop_trt_subseq1 - prop_trt_subseq2
    ) %>%
    filter(
      prop_ctl_no >= 0,
      prop_ctl_subseq1 >= 0,
      prop_ctl_subseq2 >= 0,
      prop_trt_no >= 0,
      prop_trt_subseq1 >= 0,
      prop_trt_subseq2 >= 0
    )
  check_vars <- c(
    "prop_ctl_subseq1",
    "prop_ctl_subseq2",
    "prop_trt_subseq1",
    "prop_trt_subseq2"
  )
  n_unique <- sapply(grid[check_vars], function(x) length(unique(x)))
  final_varying_vars <- names(n_unique)[n_unique > 1]
  if (length(final_varying_vars) > 1) {
    stop(
      paste0(
        "Only one variable about subsequent treatment proportions is allowed to vary, but ",
        length(final_varying_vars),
        " variables were found to vary: ",
        paste(final_varying_vars, collapse = ", ")
      )
    )
  }
  add_missing_cols <- function(df) {
    required_cols <- c(
      "hr",
      "log_hr",
      "se",
      "z",
      "p",
      "medSurvT",
      "medSurvC",
      "SurvRate12C",
      "SurvRate12T",
      "n_events",
      "n_censor",
      "censor_rate"
    )
    missing_cols <- setdiff(required_cols, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    df <- df[, required_cols, drop = FALSE]
    df
  }

  all_results <- list()


  for (r in seq_len(nrow(grid))) {

    this_ctl_no <- grid$prop_ctl_no[r]
    this_ctl_subseq1 <- grid$prop_ctl_subseq1[r]
    this_ctl_subseq2 <- grid$prop_ctl_subseq2[r]
    this_trt_no <- grid$prop_trt_no[r]
    this_trt_subseq1 <- grid$prop_trt_subseq1[r]
    this_trt_subseq2 <- grid$prop_trt_subseq2[r]

    cat(sprintf(
      ">>> Simulation %d / %d: Control No=%.2f, subseq1=%.2f, subseq2=%.2f, Treatment No=%.2f, subseq1=%.2f, subseq2=%.2f,\n ",
      r, nrow(grid), this_ctl_no, this_ctl_subseq1, this_ctl_subseq2, this_trt_no, this_trt_subseq1, this_trt_subseq2
    ))

    sim_res <- vector("list", n_simu)

    for (s in seq_len(n_simu)) {

      trial <- simulate_one_trial(
        n_total = n_total,

        median_pfs_ctl = median_pfs_ctl,
        median_pfs_trt = median_pfs_trt,

        prop_ctl_no = this_ctl_no,
        prop_ctl_subseq1 = this_ctl_subseq1,
        prop_ctl_subseq2 = this_ctl_subseq2,

        median_os_ctl_no = median_os_ctl_no,
        median_postpd_ctl_subseq1 = median_postpd_ctl_subseq1,
        median_postpd_ctl_subseq2 = median_postpd_ctl_subseq2,

        prop_trt_no = this_trt_no,
        prop_trt_subseq1 = this_trt_subseq1,
        prop_trt_subseq2 = this_trt_subseq2,

        median_os_trt_no = median_os_trt_no,
        median_postpd_trt_subseq1 = median_postpd_trt_subseq1,
        median_postpd_trt_subseq2 = median_postpd_trt_subseq2,

        interim_events = interim_events,
        final_events = final_events,

        target_censor_rate = target_censor_rate,
        seed = r * 100000 + s,
        enroll_duration = enroll_duration
      )

      ana_interim <- analyse_trial(trial$interim, "os_time_interim", "os_status_interim")
      ana_final   <- analyse_trial(trial$final,   "os_time_final",   "os_status_final")

      ana_interim <- add_missing_cols(ana_interim)
      ana_final <- add_missing_cols(ana_final)


      interim_success <- with(
        ana_interim,
        p < alpha_interim & hr < 1
      )

      final_success <- with(
        ana_final,
        p < alpha_final & hr < 1
      )
      cond_final_success <- final_success & (!interim_success)

      overall_success <- interim_success | final_success

      sim_res[[s]] <- data.frame(
        sim = s,

        prop_ctl_no = this_ctl_no,
        prop_ctl_subseq1 = this_ctl_subseq1,
        prop_ctl_subseq2 = this_ctl_subseq2,

        prop_trt_no = this_trt_no,
        prop_trt_subseq1 = this_trt_subseq1,
        prop_trt_subseq2 = this_trt_subseq2,

        hr_interim = ana_interim$hr,
        p_interim = ana_interim$p,
        medSurvT_interim = ana_interim$medSurvT,
        medSurvC_interim = ana_interim$medSurvC,
        SurvRate12T_interim = ana_interim$SurvRate12T,
        SurvRate12C_interim = ana_interim$SurvRate12C,
        # SurvRate24T_interim = ana_interim$SurvRate24T,
        # SurvRate24C_interim = ana_interim$SurvRate24C,
        censor_interim = ana_interim$censor_rate,
        interim_success = interim_success,

        hr_final = ana_final$hr,
        p_final = ana_final$p,
        medSurvT_final = ana_final$medSurvT,
        medSurvC_final = ana_final$medSurvC,
        SurvRate12T_final = ana_final$SurvRate12T,
        SurvRate12C_final = ana_final$SurvRate12C,
        # SurvRate24T_final = ana_final$SurvRate24T,
        # SurvRate24C_final = ana_final$SurvRate24C,
        censor_final = ana_final$censor_rate,
        cond_final_success = cond_final_success,

        overall_success = overall_success,

        final_hr_lt = ana_final$hr < hr_thr
      )
    }

    sim_res <- bind_rows(sim_res)

    summary_res <- sim_res %>%
      summarise(
        n_simu = n(),

        prop_ctl_no = first(prop_ctl_no),
        prop_ctl_subseq1 = first(prop_ctl_subseq1),
        prop_ctl_subseq2 = first(prop_ctl_subseq2),
        prop_trt_no = first(prop_trt_no),
        prop_trt_subseq1 = first(prop_trt_subseq1),
        prop_trt_subseq2 = first(prop_trt_subseq2),

        mean_hr_final = mean(hr_final, na.rm = TRUE),
        median_hr_final = median(hr_final, na.rm = TRUE),
        sd_hr_final = sd(hr_final, na.rm = TRUE),

        mean_hr_final = mean(hr_final, na.rm = TRUE),
        mean_medSurvT_final = mean(medSurvT_final, na.rm = TRUE),
        mean_medSurvC_final = mean(medSurvC_final, na.rm = TRUE),
        mean_SurvRate12T_final = mean(SurvRate12T_final, na.rm = TRUE),
        mean_SurvRate12C_final = mean(SurvRate12C_final, na.rm = TRUE),

        prob_hr_lt = mean(final_hr_lt, na.rm = TRUE),

        POS = mean(overall_success, na.rm = TRUE),
        final_CondPOS = mean(cond_final_success, na.rm = TRUE),
        interim_POS = mean(interim_success, na.rm = TRUE),

        mean_p_final = mean(p_final, na.rm = TRUE),
        median_p_final = median(p_final, na.rm = TRUE),

        mean_censor_interim = mean(censor_interim, na.rm = TRUE),
        mean_censor_final = mean(censor_final, na.rm = TRUE)
      )

    all_results[[r]] <- list(
      # detail = sim_res,
      summary = summary_res
    )
  }

  summary_all <- bind_rows(lapply(all_results, function(x) x$summary))
  # detail_all <- bind_rows(lapply(all_results, function(x) x$detail))

  list(
    # detail = detail_all,
    summary = summary_all
  )
}





# 
# run_grid_simulation <- function(
#     n_simu = 1000,
#     n_total = 282,
#     interim_events = 138,
#     final_events = 197,
# 
#     alpha_interim = 0.0147,
#     alpha_final = 0.04551,
# 
#     median_pfs_ctl = 3,
#     median_pfs_trt = 14.6,
# 
#     median_os_ctl_no = 9.5,
#     median_postpd_ctl_subseq1 = 22,
#     median_postpd_ctl_subseq2 = 15,
# 
#     median_os_trt_no = 25,
#     median_postpd_trt_subseq1 = 22,
#     median_postpd_trt_subseq2 = 15,
# 
#     prop_ctl_subseq1 = 0.15,
#     prop_ctl_subseq2 = seq(0.10, 0.80, by = 0.05),
# 
#     prop_trt_subseq1 = 0.05,
#     prop_trt_subseq2 = 0.05,
# 
#     hr_thr = 0.8,
#     enroll_duration = 11,
#     target_censor_rate = 0.29,
#     seed = 20260427
# ) {
# 
#   body <- list(
#     n_simu = n_simu,
#     n_total = n_total,
#     interim_events = interim_events,
#     final_events = final_events,
# 
#     alpha_interim = alpha_interim,
#     alpha_final = alpha_final,
# 
#     median_pfs_ctl = median_pfs_ctl,
#     median_pfs_trt = median_pfs_trt,
# 
#     median_os_ctl_no = median_os_ctl_no,
#     median_postpd_ctl_subseq1 = median_postpd_ctl_subseq1,
#     median_postpd_ctl_subseq2 = median_postpd_ctl_subseq2,
# 
#     median_os_trt_no = median_os_trt_no,
#     median_postpd_trt_subseq1 = median_postpd_trt_subseq1,
#     median_postpd_trt_subseq2 = median_postpd_trt_subseq2,
# 
#     prop_ctl_subseq1 = prop_ctl_subseq1,
#     prop_ctl_subseq2 = prop_ctl_subseq2,
# 
#     prop_trt_subseq1 = prop_trt_subseq1,
#     prop_trt_subseq2 = prop_trt_subseq2,
# 
#     hr_thr = hr_thr,
#     enroll_duration = enroll_duration,
#     target_censor_rate = target_censor_rate,
#     seed = seed
#   )
# 
#   result <- .pos_api_post(
#     endpoint = "/run_grid_simulation",
#     body = body,
#     timeout_sec = 36000
#   )
# 
#   result
# }
