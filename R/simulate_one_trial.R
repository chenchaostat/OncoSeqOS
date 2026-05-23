#' @title Simulate One Oncology Trial
#' @description
#' Calls server API to simulate one oncology clinical trial with interim and final analyses.
#'
#' @param n_total Total sample size.
#' @param median_pfs_ctl Median PFS in control arm.
#' @param median_pfs_trt Median PFS in treatment arm.
#' @param prop_ctl_no Proportion of control patients without subsequent therapy.
#' @param prop_ctl_subseq1 Proportion of control patients receiving subsequent therapy 1.
#' @param prop_ctl_subseq2 Proportion of control patients receiving subsequent therapy 2.
#' @param median_os_ctl_no Median OS in control patients without subsequent therapy.
#' @param median_postpd_ctl_subseq1 Median post-PD survival for control subseq1.
#' @param median_postpd_ctl_subseq2 Median post-PD survival for control subseq2.
#' @param prop_trt_no Proportion of treatment patients without subsequent therapy.
#' @param prop_trt_subseq1 Proportion of treatment patients receiving subsequent therapy 1.
#' @param prop_trt_subseq2 Proportion of treatment patients receiving subsequent therapy 2.
#' @param median_os_trt_no Median OS in treatment patients without subsequent therapy.
#' @param median_postpd_trt_subseq1 Median post-PD survival for treatment subseq1.
#' @param median_postpd_trt_subseq2 Median post-PD survival for treatment subseq2.
#' @param interim_events Number of events at interim analysis.
#' @param final_events Number of events at final analysis.
#' @param target_censor_rate Target censoring rate.
#' @param seed Random seed.
#' @param enroll_duration Enrollment duration.
#'
#' @returns A list containing full, interim, final datasets and trial-level summary values.
#' @export
simulate_one_trial <- function(
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
    seed = 20260427,
    enroll_duration = 36
) {
  
  body <- list(
    n_total = n_total,
    
    median_pfs_ctl = median_pfs_ctl,
    median_pfs_trt = median_pfs_trt,
    
    prop_ctl_no = prop_ctl_no,
    prop_ctl_subseq1 = prop_ctl_subseq1,
    prop_ctl_subseq2 = prop_ctl_subseq2,
    
    median_os_ctl_no = median_os_ctl_no,
    median_postpd_ctl_subseq1 = median_postpd_ctl_subseq1,
    median_postpd_ctl_subseq2 = median_postpd_ctl_subseq2,
    
    prop_trt_no = prop_trt_no,
    prop_trt_subseq1 = prop_trt_subseq1,
    prop_trt_subseq2 = prop_trt_subseq2,
    
    median_os_trt_no = median_os_trt_no,
    median_postpd_trt_subseq1 = median_postpd_trt_subseq1,
    median_postpd_trt_subseq2 = median_postpd_trt_subseq2,
    
    interim_events = interim_events,
    final_events = final_events,
    
    target_censor_rate = target_censor_rate,
    seed = seed,
    enroll_duration = enroll_duration
  )
  
  result <- .pos_api_post(
    endpoint = "/simulate_one_trial",
    body = body,
    timeout_sec = 600
  )
  
  result
}
