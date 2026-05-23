#' @title Hazard rate lambda from median survival time
#'
#' @description
#' Calls server API to calculate hazard rate lambda from median survival time
#' for oncology clinical trial analysis.
#'
#' @param median Median survival time. Must be a positive numeric value or vector.
#'
#' @returns A numeric value, numeric vector, or a list returned by the server API
#' containing the calculated hazard rate lambda.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lambda_from_median(12)
#'
#' lambda_from_median(c(3, 6, 12))
#' }
lambda_from_median <- function(median) {
  
  body <- list(
    median = median
  )
  
  result <- .pos_api_post(
    endpoint = "/lambda_from_median",
    body = body,
    timeout_sec = 60
  )
  
  result
}
