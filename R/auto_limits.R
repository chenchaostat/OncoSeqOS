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

auto_limits <- function(x, expand = 0.06, hard_limits = NULL, include_zero = FALSE) {
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(c(0, 1))
  }
  
  if (!is.null(hard_limits)) {
    return(hard_limits)
  }
  
  rng <- range(x, na.rm = TRUE)
  span <- diff(rng)
  
  if (span == 0) {
    span <- abs(rng[1]) * 0.1
    if (span == 0) span <- 1
  }
  
  lower <- rng[1] - span * expand
  upper <- rng[2] + span * expand
  
  if (include_zero) {
    lower <- min(0, lower)
  }
  
  c(lower, upper)
}

# 
# auto_limits <- function(x,
#                         expand = 0.06,
#                         hard_limits = NULL,
#                         include_zero = FALSE) {
#   
#   body <- list(
#     x = x,
#     expand = expand,
#     hard_limits = hard_limits,
#     include_zero = include_zero
#   )
#   
#   result <- .pos_api_post(
#     endpoint = "/auto_limits",
#     body = body,
#     timeout_sec = 60
#   )
#   
#   as.numeric(result)
# }
