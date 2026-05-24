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



run_grid_simulation_resumable <- function(
    n_simu = 1000,
    batch_size = 50,
    max_retry = 3,
    checkpoint_file = "grid_simulation_checkpoint.rds",
    
    # 新增：运行模式
    resume_mode = c("resume", "restart", "overwrite"),
    
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
    seed = 20260427,
    
    timeout_sec = 1800
) {
  
  resume_mode <- match.arg(resume_mode)
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
  })
  
  # -----------------------------
  # 1. 客户端构建 grid
  # -----------------------------
  make_grid <- function() {
    
    params <- list(
      prop_ctl_subseq1 = prop_ctl_subseq1,
      prop_ctl_subseq2 = prop_ctl_subseq2,
      prop_trt_subseq1 = prop_trt_subseq1,
      prop_trt_subseq2 = prop_trt_subseq2
    )
    
    varying_vars <- names(params)[sapply(params, length) > 1]
    fixed_vars   <- names(params)[sapply(params, length) == 1]
    
    if (length(varying_vars) == 0) {
      grid <- tibble(dummy = 1)
    } else {
      grid <- tibble(
        !!varying_vars[1] := params[[varying_vars[1]]]
      )
    }
    
    for (v in fixed_vars) {
      grid[[v]] <- params[[v]]
    }
    
    if ("dummy" %in% names(grid)) {
      grid$dummy <- NULL
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
    
    grid
  }
  
  grid <- make_grid()
  n_scenarios <- nrow(grid)
  
  cat(sprintf("Total scenarios: %d\n", n_scenarios))
  cat(sprintf("Total simulations per scenario: %d\n", n_simu))
  cat(sprintf("Batch size: %d\n", batch_size))
  cat(sprintf("Resume mode: %s\n", resume_mode))
  
  # -----------------------------
  # 2. 当前运行参数元信息
  # -----------------------------
  current_meta <- list(
    n_simu = n_simu,
    batch_size = batch_size,
    
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
  
  # -----------------------------
  # 3. 根据 resume_mode 处理 checkpoint
  # -----------------------------
  
  if (resume_mode == "restart") {
    if (file.exists(checkpoint_file)) {
      cat(sprintf("Restart mode: removing old checkpoint: %s\n", checkpoint_file))
      file.remove(checkpoint_file)
    }
    
    detail_all <- list()
    finished_keys <- character(0)
    checkpoint_meta <- current_meta
  } else {
    
    if (file.exists(checkpoint_file)) {
      
      cat(sprintf("Loading checkpoint: %s\n", checkpoint_file))
      checkpoint <- readRDS(checkpoint_file)
      
      detail_all <- checkpoint$detail
      finished_keys <- checkpoint$finished_keys
      
      if (!is.null(checkpoint$meta)) {
        checkpoint_meta <- checkpoint$meta
        
        if (!identical(checkpoint_meta, current_meta)) {
          warning(
            paste0(
              "\nThe existing checkpoint was created with different parameters.\n",
              "If you want to recompute from scratch, use resume_mode = 'restart'.\n",
              "If you want to overwrite existing batches, use resume_mode = 'overwrite'.\n"
            )
          )
        }
        
      } else {
        warning(
          "Old checkpoint has no meta information. Please consider using resume_mode = 'restart'."
        )
        checkpoint_meta <- current_meta
      }
      
    } else {
      
      detail_all <- list()
      finished_keys <- character(0)
      checkpoint_meta <- current_meta
    }
  }
  
  # -----------------------------
  # 4. 单个 batch 调用函数，带重试
  # -----------------------------
  call_one_batch <- function(scenario_index, sim_start) {
    
    body <- list(
      n_simu = n_simu,
      scenario_index = scenario_index,
      sim_start = sim_start,
      batch_size = batch_size,
      
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
    
    last_error <- NULL
    
    for (attempt in seq_len(max_retry)) {
      
      cat(sprintf(
        "Calling scenario %d, sim_start %d, attempt %d / %d ...\n",
        scenario_index, sim_start, attempt, max_retry
      ))
      
      res <- tryCatch(
        {
          .pos_api_post(
            endpoint = "/run_grid_simulation_batch",
            body = body,
            timeout_sec = timeout_sec
          )
        },
        error = function(e) {
          last_error <<- e
          NULL
        }
      )
      
      if (!is.null(res)) {
        return(res)
      }
      
      wait_sec <- min(10 * attempt, 60)
      cat(sprintf("Batch failed. Waiting %d seconds before retry...\n", wait_sec))
      Sys.sleep(wait_sec)
    }
    
    stop(last_error)
  }
  
  # -----------------------------
  # 5. 主循环：scenario × batch
  # -----------------------------
  for (scenario_index in seq_len(n_scenarios)) {
    
    sim_starts <- seq(1, n_simu, by = batch_size)
    
    for (sim_start in sim_starts) {
      
      key <- paste0("scenario_", scenario_index, "_sim_", sim_start)
      
      # 关键修改：
      # resume 模式下：已经完成的跳过
      # overwrite 模式下：即使已经完成，也重新计算并覆盖
      # restart 模式下：前面已删除 checkpoint，因此自然从头算
      if (resume_mode == "resume" && key %in% finished_keys) {
        cat(sprintf("Skipping finished batch: %s\n", key))
        next
      }
      
      if (resume_mode == "overwrite" && key %in% finished_keys) {
        cat(sprintf("Overwriting finished batch: %s\n", key))
      }
      
      res <- call_one_batch(
        scenario_index = scenario_index,
        sim_start = sim_start
      )
      
      detail_batch <- as.data.frame(res$detail)
      
      # 这里会覆盖同名 key 的旧结果
      detail_all[[key]] <- detail_batch
      
      # 防止 finished_keys 重复累积
      finished_keys <- unique(c(finished_keys, key))
      
      saveRDS(
        list(
          detail = detail_all,
          finished_keys = finished_keys,
          meta = current_meta
        ),
        checkpoint_file
      )
      
      cat(sprintf("Saved checkpoint after %s\n", key))
    }
  }
  
  # -----------------------------
  # 6. 合并所有 detail
  # -----------------------------
  detail_all_df <- bind_rows(detail_all)
  
  # -----------------------------
  # 7. 最终 summary
  # -----------------------------
  summary_all <- detail_all_df %>%
    group_by(
      scenario_index,
      prop_ctl_no,
      prop_ctl_subseq1,
      prop_ctl_subseq2,
      prop_trt_no,
      prop_trt_subseq1,
      prop_trt_subseq2
    ) %>%
    summarise(
      n_simu = n(),
      
      mean_hr_final = mean(hr_final, na.rm = TRUE),
      median_hr_final = median(hr_final, na.rm = TRUE),
      sd_hr_final = sd(hr_final, na.rm = TRUE),
      
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
      mean_censor_final = mean(censor_final, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    arrange(scenario_index)
  
  list(
    summary = summary_all,
    detail = detail_all_df,
    checkpoint_file = checkpoint_file,
    resume_mode = resume_mode
  )
}






# 
# run_grid_simulation <- function(
#     n_simu = 1000,
#     n_total = 282,
#     interim_events = 138,
#     final_events = 197,
#     alpha_interim = 0.0147,
#     alpha_final = 0.04551,
#     median_pfs_ctl = 3,
#     median_pfs_trt = 14.6,
#     median_os_ctl_no = 9.5,
#     median_postpd_ctl_subseq1 = 22,
#     median_postpd_ctl_subseq2 = 15,
#     median_os_trt_no = 25,
#     median_postpd_trt_subseq1 = 22,
#     median_postpd_trt_subseq2 = 15,
#     prop_ctl_subseq1 = 0.15,
#     prop_ctl_subseq2 = seq(0.10, 0.80, by = 0.05),
#     prop_trt_subseq1 = 0.05,
#     prop_trt_subseq2 = 0.05,
#     hr_thr = 0.8,
#     enroll_duration = 11,
#     target_censor_rate = 0.29,
#     seed = 20260427
# ) {
#   
#   set.seed(seed)
#   params <- list(
#     prop_ctl_subseq1 = prop_ctl_subseq1,
#     prop_ctl_subseq2 = prop_ctl_subseq2,
#     prop_trt_subseq1 = prop_trt_subseq1,
#     prop_trt_subseq2 = prop_trt_subseq2
#   )
#   varying_vars <- names(params)[sapply(params, length) > 1]
#   fixed_vars <- names(params)[sapply(params, length) == 1]
#   grid <- tibble(
#     !!varying_vars[1] := params[[varying_vars[1]]]
#   )
#   for (v in fixed_vars) {
#     grid[[v]] <- params[[v]]
#   }
#   grid <- grid %>%
#     mutate(
#       prop_ctl_no = 1 - prop_ctl_subseq1 - prop_ctl_subseq2,
#       prop_trt_no = 1 - prop_trt_subseq1 - prop_trt_subseq2
#     ) %>%
#     filter(
#       prop_ctl_no >= 0,
#       prop_ctl_subseq1 >= 0,
#       prop_ctl_subseq2 >= 0,
#       prop_trt_no >= 0,
#       prop_trt_subseq1 >= 0,
#       prop_trt_subseq2 >= 0
#     )
#   check_vars <- c(
#     "prop_ctl_subseq1",
#     "prop_ctl_subseq2",
#     "prop_trt_subseq1",
#     "prop_trt_subseq2"
#   )
#   n_unique <- sapply(grid[check_vars], function(x) length(unique(x)))
#   final_varying_vars <- names(n_unique)[n_unique > 1]
#   if (length(final_varying_vars) > 1) {
#     stop(
#       paste0(
#         "Only one variable about subsequent treatment proportions is allowed to vary, but ",
#         length(final_varying_vars),
#         " variables were found to vary: ",
#         paste(final_varying_vars, collapse = ", ")
#       )
#     )
#   }
#   add_missing_cols <- function(df) {
#     required_cols <- c(
#       "hr",
#       "log_hr",
#       "se",
#       "z",
#       "p",
#       "medSurvT",
#       "medSurvC",
#       "SurvRate12C",
#       "SurvRate12T",
#       "n_events",
#       "n_censor",
#       "censor_rate"
#     )
#     missing_cols <- setdiff(required_cols, names(df))
#     for (col in missing_cols) {
#       df[[col]] <- NA
#     }
#     df <- df[, required_cols, drop = FALSE]
#     df
#   }
# 
#   all_results <- list()
#   
# 
#   for (r in seq_len(nrow(grid))) {
#     
#     this_ctl_no <- grid$prop_ctl_no[r]
#     this_ctl_subseq1 <- grid$prop_ctl_subseq1[r]
#     this_ctl_subseq2 <- grid$prop_ctl_subseq2[r]
#     this_trt_no <- grid$prop_trt_no[r]
#     this_trt_subseq1 <- grid$prop_trt_subseq1[r]
#     this_trt_subseq2 <- grid$prop_trt_subseq2[r]
#     
#     cat(sprintf(
#       ">>> Simulation %d / %d: Control No=%.2f, subseq1=%.2f, subseq2=%.2f, Treatment No=%.2f, subseq1=%.2f, subseq2=%.2f,\n ",
#       r, nrow(grid), this_ctl_no, this_ctl_subseq1, this_ctl_subseq2, this_trt_no, this_trt_subseq1, this_trt_subseq2
#     ))
#     
#     sim_res <- vector("list", n_simu)
#     
#     for (s in seq_len(n_simu)) {
#       
#       trial <- simulate_one_trial(
#         n_total = n_total,
#         
#         median_pfs_ctl = median_pfs_ctl,
#         median_pfs_trt = median_pfs_trt,
#         
#         prop_ctl_no = this_ctl_no,
#         prop_ctl_subseq1 = this_ctl_subseq1,
#         prop_ctl_subseq2 = this_ctl_subseq2,
#         
#         median_os_ctl_no = median_os_ctl_no,
#         median_postpd_ctl_subseq1 = median_postpd_ctl_subseq1,
#         median_postpd_ctl_subseq2 = median_postpd_ctl_subseq2,
#         
#         prop_trt_no = this_trt_no,
#         prop_trt_subseq1 = this_trt_subseq1,
#         prop_trt_subseq2 = this_trt_subseq2,
#         
#         median_os_trt_no = median_os_trt_no,
#         median_postpd_trt_subseq1 = median_postpd_trt_subseq1,
#         median_postpd_trt_subseq2 = median_postpd_trt_subseq2,
#         
#         interim_events = interim_events,
#         final_events = final_events,
#         
#         target_censor_rate = target_censor_rate,
#         seed = r * 100000 + s,
#         enroll_duration = enroll_duration
#       )
#       
#       ana_interim <- analyse_trial(trial$interim, "os_time_interim", "os_status_interim")
#       ana_final   <- analyse_trial(trial$final,   "os_time_final",   "os_status_final")
# 
#       ana_interim <- add_missing_cols(ana_interim)
#       ana_final <- add_missing_cols(ana_final)
#       
#       
#       interim_success <- with(
#         ana_interim,
#         p < alpha_interim & hr < 1
#       )
#       
#       final_success <- with(
#         ana_final,
#         p < alpha_final & hr < 1
#       )
#       cond_final_success <- final_success & (!interim_success)
# 
#       overall_success <- interim_success | final_success
#       
#       sim_res[[s]] <- data.frame(
#         sim = s,
#         
#         prop_ctl_no = this_ctl_no,
#         prop_ctl_subseq1 = this_ctl_subseq1,
#         prop_ctl_subseq2 = this_ctl_subseq2,
#         
#         prop_trt_no = this_trt_no,
#         prop_trt_subseq1 = this_trt_subseq1,
#         prop_trt_subseq2 = this_trt_subseq2,
#         
#         hr_interim = ana_interim$hr,
#         p_interim = ana_interim$p,
#         medSurvT_interim = ana_interim$medSurvT,
#         medSurvC_interim = ana_interim$medSurvC,
#         SurvRate12T_interim = ana_interim$SurvRate12T,
#         SurvRate12C_interim = ana_interim$SurvRate12C,
#         # SurvRate24T_interim = ana_interim$SurvRate24T,
#         # SurvRate24C_interim = ana_interim$SurvRate24C,
#         censor_interim = ana_interim$censor_rate,
#         interim_success = interim_success,
#         
#         hr_final = ana_final$hr,
#         p_final = ana_final$p,
#         medSurvT_final = ana_final$medSurvT,
#         medSurvC_final = ana_final$medSurvC,
#         SurvRate12T_final = ana_final$SurvRate12T,
#         SurvRate12C_final = ana_final$SurvRate12C,
#         # SurvRate24T_final = ana_final$SurvRate24T,
#         # SurvRate24C_final = ana_final$SurvRate24C,
#         censor_final = ana_final$censor_rate,
#         cond_final_success = cond_final_success,
#         
#         overall_success = overall_success,
#         
#         final_hr_lt = ana_final$hr < hr_thr
#       )
#     }
#     
#     sim_res <- bind_rows(sim_res)
#     
#     summary_res <- sim_res %>%
#       summarise(
#         n_simu = n(),
#         
#         prop_ctl_no = first(prop_ctl_no),
#         prop_ctl_subseq1 = first(prop_ctl_subseq1),
#         prop_ctl_subseq2 = first(prop_ctl_subseq2),
#         prop_trt_no = first(prop_trt_no),
#         prop_trt_subseq1 = first(prop_trt_subseq1),
#         prop_trt_subseq2 = first(prop_trt_subseq2),
#         
#         mean_hr_final = mean(hr_final, na.rm = TRUE),
#         median_hr_final = median(hr_final, na.rm = TRUE),
#         sd_hr_final = sd(hr_final, na.rm = TRUE),
#         
#         mean_hr_final = mean(hr_final, na.rm = TRUE),
#         mean_medSurvT_final = mean(medSurvT_final, na.rm = TRUE),
#         mean_medSurvC_final = mean(medSurvC_final, na.rm = TRUE),
#         mean_SurvRate12T_final = mean(SurvRate12T_final, na.rm = TRUE),
#         mean_SurvRate12C_final = mean(SurvRate12C_final, na.rm = TRUE),
#         
#         prob_hr_lt = mean(final_hr_lt, na.rm = TRUE),
#         
#         POS = mean(overall_success, na.rm = TRUE),
#         final_CondPOS = mean(cond_final_success, na.rm = TRUE),
#         interim_POS = mean(interim_success, na.rm = TRUE),
#         
#         mean_p_final = mean(p_final, na.rm = TRUE),
#         median_p_final = median(p_final, na.rm = TRUE),
#         
#         mean_censor_interim = mean(censor_interim, na.rm = TRUE),
#         mean_censor_final = mean(censor_final, na.rm = TRUE)
#       )
#     
#     all_results[[r]] <- list(
#       # detail = sim_res,
#       summary = summary_res
#     )
#   }
#   
#   summary_all <- bind_rows(lapply(all_results, function(x) x$summary))
#   # detail_all <- bind_rows(lapply(all_results, function(x) x$detail))
#   
#   list(
#     # detail = detail_all,
#     summary = summary_all
#   )
# }





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
