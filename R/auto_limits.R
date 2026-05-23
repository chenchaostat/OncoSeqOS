#' @title Automatically Determine Axis Limits
#' @description Calls server API to determine axis limits.
#'
#' @param x Numeric vector.
#' @param expand Expansion ratio.
#' @param hard_limits Optional hard limits.
#' @param include_zero Whether to include zero.
#'
#' @returns Numeric vector of length 2.
#' @export
auto_limits <- function(x,
                        expand = 0.06,
                        hard_limits = NULL,
                        include_zero = FALSE) {
  
  body <- list(
    x = x,
    expand = expand,
    hard_limits = hard_limits,
    include_zero = include_zero
  )
  
  result <- .pos_api_post(
    endpoint = "/auto_limits",
    body = body,
    timeout_sec = 60
  )
  
  as.numeric(result)
}
