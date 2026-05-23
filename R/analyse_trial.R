#' @title Analyse Simulated Trial
#' @description
#' Calls server API to run Cox model and survival summaries.
#'
#' @param df Input trial dataset.
#' @param time_var Time variable name.
#' @param status_var Status variable name.
#'
#' @returns A data.frame containing HR, p-value, median survival, survival rate and censoring rate.
#' @export
analyse_trial <- function(df,
                          time_var,
                          status_var) {
  
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }
  
  body <- list(
    df = df,
    time_var = time_var,
    status_var = status_var
  )
  
  result <- .pos_api_post(
    endpoint = "/analyse_trial",
    body = body,
    timeout_sec = 300
  )
  
  as.data.frame(result)
}
