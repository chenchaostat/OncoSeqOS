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
  
  body <- list(
    n_simu = n_simu,
    n_total = n_total,
    interim_events = interim_events,
    final_events = final_events,
    
    alpha_interim = alpha_interim,
    alpha_final = alpha_final,
    
    median_pfs_ctl = median_pfs_ctl,
    median_pfs_trt = median_pfs_trt,
    
    median_os_ctl_no = median_os_ctl_no,
    median_postpd_ctl_subseq1 = median_postpd_ctl_subseq1,
    median_postpd_ctl_subseq2 = median_postpd_ctl_subseq2,
    
    median_os_trt_no = median_os_trt_no,
    median_postpd_trt_subseq1 = median_postpd_trt_subseq1,
    median_postpd_trt_subseq2 = median_postpd_trt_subseq2,
    
    prop_ctl_subseq1 = prop_ctl_subseq1,
    prop_ctl_subseq2 = prop_ctl_subseq2,
    
    prop_trt_subseq1 = prop_trt_subseq1,
    prop_trt_subseq2 = prop_trt_subseq2,
    
    hr_thr = hr_thr,
    enroll_duration = enroll_duration,
    target_censor_rate = target_censor_rate,
    seed = seed
  )
  
  result <- .pos_api_post(
    endpoint = "/run_grid_simulation",
    body = body,
    timeout_sec = 3600
  )
  
  result
}
