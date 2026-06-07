# R/api.R
#' @importFrom httr POST GET content content_type_json status_code add_headers timeout
#' @importFrom jsonlite toJSON fromJSON
NULL

.api_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  
  # -----------------------------
  # API server URL
  # -----------------------------
  url <- Sys.getenv("ONCOSEQOS_API_URL", unset = "")
  
  if (nchar(url) == 0) {
    url <- getOption("oncoseqos.api.base_url", default = "")
  }
  
  if (nchar(url) == 0) {
    url <- "http://localhost:10000"
  }
  
  .api_env$base_url <- normalize_base_url(url)
  
  # -----------------------------
  # API token
  # -----------------------------
  token <- Sys.getenv("ONCOSEQOS_API_TOKEN", unset = "")
  
  if (nchar(token) == 0) {
    token <- getOption("oncoseqos.api.token", default = "")
  }
  
  .api_env$api_token <- token
}

normalize_base_url <- function(url) {
  url <- trimws(url)
  sub("/+$", "", url)
}

#' Set OncoSeqOS API server
#'
#' @param url API server base URL, e.g. "http://116.62.190.134:10000"
#' @export
oncoseqos_set_server <- function(url) {
  
  if (missing(url) || is.null(url) || !nzchar(url)) {
    stop("Please provide a valid server URL.", call. = FALSE)
  }
  
  url <- normalize_base_url(url)
  
  .api_env$base_url <- url
  options(oncoseqos.api.base_url = url)
  
  message("OncoSeqOS API server set to: ", url)
  invisible(url)
}

#' Get current OncoSeqOS API server
#'
#' @export
oncoseqos_get_server <- function() {
  
  url <- getOption(
    "oncoseqos.api.base_url",
    default = .api_env$base_url
  )
  
  normalize_base_url(url)
}

#' Set OncoSeqOS API token
#'
#' @param token API token provided by the server owner.
#' @export
oncoseqos_set_api_token <- function(token) {
  
  if (missing(token) || is.null(token) || !nzchar(token)) {
    stop("Please provide a non-empty API token.", call. = FALSE)
  }
  
  token <- trimws(token)
  
  .api_env$api_token <- token
  options(oncoseqos.api.token = token)
  
  message("OncoSeqOS API token has been set.")
  invisible(TRUE)
}

#' Get current API token
#'
#' @param mask Whether to mask the token in display.
#' @export
oncoseqos_get_api_token <- function(mask = TRUE) {
  
  token <- getOption(
    "oncoseqos.api.token",
    default = .api_env$api_token
  )
  
  if (is.null(token) || !nzchar(token)) {
    return("")
  }
  
  if (!mask) {
    return(token)
  }
  
  n <- nchar(token)
  
  if (n <= 6) {
    return(paste0(substr(token, 1, 1), "****"))
  }
  
  paste0(substr(token, 1, 3), "****", substr(token, n - 2, n))
}

#' Set API Server URL
#'
#' @description
#' Set the base URL of the OncoSeqOS API server.
#'
#' @param url Character string. The base URL of the API server, for example
#'   `"http://116.62.190.134:10001"`.
#'
#' @return Invisibly returns the API server URL.
#'
#' @export
set_api_url <- function(url) {
  oncoseqos_set_server(url)
}


#' Get API Server URL
#'
#' @description
#' Get the current base URL of the OncoSeqOS API server.
#'
#' @return Character string. The current API server URL.
#'
#' @export
get_api_url <- function() {
  oncoseqos_get_server()
}


.pos_build_url <- function(endpoint) {
  
  base_url <- oncoseqos_get_server()
  
  if (is.null(base_url) || !nzchar(base_url)) {
    stop(
      "API server URL is not set. Please call oncoseqos_set_server('http://host:port') first.",
      call. = FALSE
    )
  }
  
  endpoint <- paste0("/", sub("^/+", "", endpoint))
  
  paste0(base_url, endpoint)
}

.pos_auth_headers <- function() {
  
  token <- getOption(
    "oncoseqos.api.token",
    default = .api_env$api_token
  )
  
  if (is.null(token) || !nzchar(token)) {
    stop(
      "API token is not set.\n",
      "Please call oncoseqos_set_api_token('<your-token>') before using this function.",
      call. = FALSE
    )
  }
  
  list(
    httr::add_headers(
      Authorization = paste("Bearer", token),
      `X-API-Key` = token
    )
  )
}

.pos_api_post <- function(endpoint, body = list(), timeout_sec = 600) {
  
  url <- .pos_build_url(endpoint)
  auth_headers <- .pos_auth_headers()
  
  response <- tryCatch({
    httr::POST(
      url = url,
      body = jsonlite::toJSON(
        body,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        dataframe = "rows"
      ),
      httr::content_type_json(),
      auth_headers[[1]],
      httr::timeout(timeout_sec),
      encode = "raw"
    )
  }, error = function(e) {
    stop(
      "Unable to connect to the API server. Please check your internet connection or server URL.\n",
      "Endpoint: ", endpoint, "\n",
      "URL: ", url, "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  status <- httr::status_code(response)
  
  result_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  if (!status %in% c(200, 201, 202)) {
    stop(
      "API request failed.\n",
      "Endpoint: ", endpoint, "\n",
      "URL: ", url, "\n",
      "Status code: ", status, "\n",
      "Response: ", result_text,
      call. = FALSE
    )
  }
  
  jsonlite::fromJSON(
    result_text,
    simplifyDataFrame = TRUE,
    simplifyVector = TRUE
  )
}

.pos_api_get <- function(endpoint, timeout_sec = 600) {
  
  url <- .pos_build_url(endpoint)
  auth_headers <- .pos_auth_headers()
  
  response <- tryCatch({
    httr::GET(
      url = url,
      auth_headers[[1]],
      httr::timeout(timeout_sec)
    )
  }, error = function(e) {
    stop(
      "Unable to connect to the API server. Please check your internet connection or server URL.\n",
      "Endpoint: ", endpoint, "\n",
      "URL: ", url, "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  status <- httr::status_code(response)
  
  result_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  if (!status %in% c(200, 201, 202)) {
    stop(
      "API request failed.\n",
      "Endpoint: ", endpoint, "\n",
      "URL: ", url, "\n",
      "Status code: ", status, "\n",
      "Response: ", result_text,
      call. = FALSE
    )
  }
  
  jsonlite::fromJSON(
    result_text,
    simplifyDataFrame = TRUE,
    simplifyVector = TRUE
  )
}

#' Check API health
#'
#' @export
oncoseqos_health <- function() {
  
  # health 是公开接口，不强制需要 token
  url <- paste0(oncoseqos_get_server(), "/health")
  
  response <- tryCatch({
    httr::GET(
      url = url,
      httr::timeout(60)
    )
  }, error = function(e) {
    stop(
      "Unable to connect to the API server.\n",
      "URL: ", url, "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  status <- httr::status_code(response)
  result_text <- httr::content(response, as = "text", encoding = "UTF-8")
  
  if (!status %in% c(200, 201, 202)) {
    stop(
      "Health check failed.\n",
      "Status code: ", status, "\n",
      "Response: ", result_text,
      call. = FALSE
    )
  }
  
  jsonlite::fromJSON(result_text, simplifyVector = TRUE)
}

#' @title Hazard rate lambda from median survival time
#'
#' @param median Median survival time. Must be positive numeric value or vector.
#' @export
lambda_from_median <- function(median) {
  
  body <- list(
    median = median
  )
  
  .pos_api_post(
    endpoint = "/lambda_from_median",
    body = body,
    timeout_sec = 70
  )
}

#' @title Median of Sum of Two Exponential Survival Times
#'
#' @param median_pfs Median PFS.
#' @param median_postpd Median post-PD survival.
#' @param method "formula" or "simulation".
#' @param n_sim Number of simulations when method = "simulation".
#' @param seed Random seed.
#' @param upper Upper bound used in numerical root finding.
#' @export
median_sum_exp <- function(median_pfs,
                           median_postpd,
                           method = c("formula", "simulation"),
                           n_sim = 100000,
                           seed = NULL,
                           upper = 200) {
  
  method <- match.arg(method)
  
  body <- list(
    median_pfs = median_pfs,
    median_postpd = median_postpd,
    method = method,
    n_sim = n_sim,
    seed = seed,
    upper = upper
  )
  
  result <- .pos_api_post(
    endpoint = "/median_sum_exp",
    body = body,
    timeout_sec = 300
  )
  
  as.data.frame(result)
}



#' Simulate One Oncology Trial
#'
#' @description
#' Simulate one oncology trial through the OncoSeqOS API.
#'
#' @param n_total Integer. Total sample size.
#' @param median_pfs_ctl Numeric. Median PFS in the control arm.
#' @param median_pfs_trt Numeric. Median PFS in the treatment arm.
#' @param prop_ctl_no Numeric. Proportion of control-arm patients without subsequent therapy.
#' @param prop_ctl_subseq1 Numeric. Proportion of control-arm patients receiving subsequent therapy 1.
#' @param prop_ctl_subseq2 Numeric. Proportion of control-arm patients receiving subsequent therapy 2.
#' @param median_os_ctl_no Numeric. Median OS for control patients without subsequent therapy.
#' @param median_postpd_ctl_subseq1 Numeric. Median post-progression survival for control patients receiving subsequent therapy 1.
#' @param median_postpd_ctl_subseq2 Numeric. Median post-progression survival for control patients receiving subsequent therapy 2.
#' @param prop_trt_no Numeric. Proportion of treatment-arm patients without subsequent therapy.
#' @param prop_trt_subseq1 Numeric. Proportion of treatment-arm patients receiving subsequent therapy 1.
#' @param prop_trt_subseq2 Numeric. Proportion of treatment-arm patients receiving subsequent therapy 2.
#' @param median_os_trt_no Numeric. Median OS for treatment patients without subsequent therapy.
#' @param median_postpd_trt_subseq1 Numeric. Median post-progression survival for treatment patients receiving subsequent therapy 1.
#' @param median_postpd_trt_subseq2 Numeric. Median post-progression survival for treatment patients receiving subsequent therapy 2.
#' @param interim_events Integer. Number of events required for interim analysis.
#' @param final_events Integer. Number of events required for final analysis.
#' @param target_censor_rate Numeric. Target censoring rate.
#' @param seed Integer. Random seed.
#' @param enroll_duration Numeric. Enrollment duration.
#'
#' @return A list returned by the API containing simulated trial results.
#'
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
  
  .pos_api_post(
    endpoint = "/simulate_one_trial",
    body = body,
    timeout_sec = 600
  )
}




#' Submit grid simulation job
#'
#' @description
#' Submit an asynchronous grid simulation job to the OncoSeqOS API server.
#'
#' @param n_simu Integer. Number of simulation replicates.
#' @param n_total Integer. Total sample size.
#' @param interim_events Integer. Number of events required for interim analysis.
#' @param final_events Integer. Number of events required for final analysis.
#' @param alpha_interim Numeric. Alpha level for interim analysis.
#' @param alpha_final Numeric. Alpha level for final analysis.
#' @param median_pfs_ctl Numeric. Median PFS in the control arm.
#' @param median_pfs_trt Numeric. Median PFS in the treatment arm.
#' @param median_os_ctl_no Numeric. Median OS for control patients without subsequent therapy.
#' @param median_postpd_ctl_subseq1 Numeric. Median post-progression survival for control patients receiving subsequent therapy 1.
#' @param median_postpd_ctl_subseq2 Numeric. Median post-progression survival for control patients receiving subsequent therapy 2.
#' @param median_os_trt_no Numeric. Median OS for treatment patients without subsequent therapy.
#' @param median_postpd_trt_subseq1 Numeric. Median post-progression survival for treatment patients receiving subsequent therapy 1.
#' @param median_postpd_trt_subseq2 Numeric. Median post-progression survival for treatment patients receiving subsequent therapy 2.
#' @param prop_ctl_subseq1 Numeric. Proportion of control-arm patients receiving subsequent therapy 1.
#' @param prop_ctl_subseq2 Numeric vector. Proportion values of control-arm patients receiving subsequent therapy 2.
#' @param prop_trt_subseq1 Numeric. Proportion of treatment-arm patients receiving subsequent therapy 1.
#' @param prop_trt_subseq2 Numeric. Proportion of treatment-arm patients receiving subsequent therapy 2.
#' @param hr_thr Numeric. Hazard ratio threshold.
#' @param enroll_duration Numeric. Enrollment duration.
#' @param target_censor_rate Numeric. Target censoring rate.
#' @param seed Integer. Random seed.
#'
#' @return A list containing job submission information, including job ID and status/result URLs.
#'
#' @export

submit_grid_simulation <- function(
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
  
  .pos_api_post(
    endpoint = "/submit",
    body = body,
    timeout_sec = 60
  )
}


#' Get Job Status
#'
#' @description
#' Query the current status of an asynchronous simulation job.
#'
#' @param job_id Character string. Job ID returned by `submit_grid_simulation()`
#'   or `run_grid_simulation(wait = FALSE)`.
#'
#' @return A list containing job status information.
#'
#' @export
get_job_status <- function(job_id) {
  .pos_api_get(
    endpoint = paste0("/status/", job_id),
    timeout_sec = 60
  )
}


#' Get Job Result
#'
#' @description
#' Retrieve the result of an asynchronous simulation job.
#'
#' @param job_id Character string. Job ID returned by `submit_grid_simulation()`
#'   or `run_grid_simulation(wait = FALSE)`.
#'
#' @return A list containing the job result.
#'
#' @export
get_job_result <- function(job_id) {
  
  result <- .pos_api_get(
    endpoint = paste0("/result/", job_id),
    timeout_sec = 300
  )
  
  if (!is.null(result$status) && !identical(result$status, "finished") && is.null(result$summary)) {
    warning(
      "Job is not finished yet. Please call get_job_status(job_id) or wait_for_job(job_id).",
      call. = FALSE
    )
  }
  
  result
}


#' Wait for async job
#'
#' @description
#' Poll the API server until an asynchronous job finishes, fails, or times out.
#'
#' @param job_id Character string. Job ID returned by `submit_grid_simulation()`.
#' @param interval_sec Numeric. Polling interval in seconds.
#' @param max_wait_sec Numeric. Maximum waiting time in seconds.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return Invisibly returns `TRUE` if the job finishes successfully.
#'
#' @export
wait_for_job <- function(job_id,
                         interval_sec = 10,
                         max_wait_sec = 36000,
                         verbose = TRUE) {
  
  start_time <- Sys.time()
  
  repeat {
    
    status <- get_job_status(job_id)
    
    if (verbose) {
      progress_text <- if (!is.null(status$progress) && !is.na(status$progress)) {
        paste0(round(status$progress * 100, 1), "%")
      } else {
        "NA"
      }
      
      done_text <- if (!is.null(status$done) && !is.na(status$done)) {
        as.character(status$done)
      } else {
        "NA"
      }
      
      total_text <- if (!is.null(status$total) && !is.na(status$total)) {
        as.character(status$total)
      } else {
        "NA"
      }
      
      message(
        "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
        "job_id=", job_id,
        ", status=", status$status,
        ", progress=", progress_text,
        ", done=", done_text,
        ", total=", total_text
      )
    }
    
    if (identical(status$status, "finished")) {
      return(TRUE)
    }
    
    if (identical(status$status, "failed")) {
      error_msg <- if (!is.null(status$error)) status$error else "Unknown error"
      stop(
        "Simulation job failed.\n",
        "job_id: ", job_id, "\n",
        "error: ", error_msg,
        call. = FALSE
      )
    }
    
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    if (elapsed > max_wait_sec) {
      stop(
        "Simulation job did not finish within max_wait_sec.\n",
        "job_id: ", job_id, "\n",
        "max_wait_sec: ", max_wait_sec, "\n",
        "You can continue checking later with get_job_status(job_id) and get_job_result(job_id).",
        call. = FALSE
      )
    }
    
    Sys.sleep(interval_sec)
  }
}



#' Run Grid Simulation Through API
#'
#' @description
#' Submit a grid simulation job to the OncoSeqOS API server. The function can
#' either return immediately after submission or wait until the job finishes.
#'
#' @param n_simu Integer. Number of simulation replicates.
#' @param n_total Integer. Total sample size.
#' @param interim_events Integer. Number of events required for interim analysis.
#' @param final_events Integer. Number of events required for final analysis.
#' @param alpha_interim Numeric. Alpha level for interim analysis.
#' @param alpha_final Numeric. Alpha level for final analysis.
#' @param median_pfs_ctl Numeric. Median PFS in the control arm.
#' @param median_pfs_trt Numeric. Median PFS in the treatment arm.
#' @param median_os_ctl_no Numeric. Median OS for control patients without subsequent therapy.
#' @param median_postpd_ctl_subseq1 Numeric. Median post-progression survival for control patients receiving subsequent therapy 1.
#' @param median_postpd_ctl_subseq2 Numeric. Median post-progression survival for control patients receiving subsequent therapy 2.
#' @param median_os_trt_no Numeric. Median OS for treatment patients without subsequent therapy.
#' @param median_postpd_trt_subseq1 Numeric. Median post-progression survival for treatment patients receiving subsequent therapy 1.
#' @param median_postpd_trt_subseq2 Numeric. Median post-progression survival for treatment patients receiving subsequent therapy 2.
#' @param prop_ctl_subseq1 Numeric. Proportion of control-arm patients receiving subsequent therapy 1.
#' @param prop_ctl_subseq2 Numeric vector. Proportion values of control-arm patients receiving subsequent therapy 2.
#' @param prop_trt_subseq1 Numeric. Proportion of treatment-arm patients receiving subsequent therapy 1.
#' @param prop_trt_subseq2 Numeric. Proportion of treatment-arm patients receiving subsequent therapy 2.
#' @param hr_thr Numeric. Hazard ratio threshold.
#' @param enroll_duration Numeric. Enrollment duration.
#' @param target_censor_rate Numeric. Target censoring rate.
#' @param seed Integer. Random seed.
#' @param wait Logical. Whether to wait until the submitted job finishes.
#' @param interval_sec Numeric. Polling interval in seconds when `wait = TRUE`.
#' @param max_wait_sec Numeric. Maximum waiting time in seconds.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return
#' If `wait = FALSE`, returns job submission information. If `wait = TRUE`,
#' returns the finished job result.
#'
#' @export
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
    seed = 20260427,
    
    wait = TRUE,
    interval_sec = 10,
    max_wait_sec = 36000,
    verbose = TRUE
) {
  
  submitted <- submit_grid_simulation(
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
  
  job_id <- submitted$job_id
  
  if (verbose) {
    message("Simulation job submitted.")
    message("job_id: ", job_id)
    message("status_url: ", submitted$status_url)
    message("result_url: ", submitted$result_url)
  }
  
  if (!wait) {
    return(submitted)
  }
  
  wait_for_job(
    job_id = job_id,
    interval_sec = interval_sec,
    max_wait_sec = max_wait_sec,
    verbose = verbose
  )
  
  get_job_result(job_id)
}
