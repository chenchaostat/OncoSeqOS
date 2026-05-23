#' @title Median of Sum of Two Exponential Survival Times
#' @description
#' Calls server API to calculate the median of total OS defined as
#' PFS plus post-PD survival, assuming both are independent exponential distributions.
#'
#' @param median_pfs Median PFS.
#' @param median_postpd Median post-PD survival.
#' @param method Calculation method, either "formula" or "simulation".
#' @param n_sim Number of simulations when method = "simulation".
#' @param seed Random seed.
#' @param upper Upper bound used in numerical root finding.
#'
#' @returns A data.frame containing median total OS and related statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' median_sum_exp(
#'   median_pfs = 3,
#'   median_postpd = 27,
#'   method = "formula"
#' )
#'
#' median_sum_exp(
#'   median_pfs = 3,
#'   median_postpd = 27,
#'   method = "simulation",
#'   n_sim = 100000,
#'   seed = 2024
#' )
#' }
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
