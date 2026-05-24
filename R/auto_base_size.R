#' @title Automatically Determine Base Font Size
#' @description Calls server API to determine base font size.
#'
#' @param width Plot width.
#' @param height Plot height.
#' @param min_size Minimum font size.
#' @param max_size Maximum font size.
#'
#' @returns Numeric base font size.
#' @export




auto_base_size <- function(width = 10, height = 6, min_size = 10, max_size = 16) {
  size <- 11 + 0.45 * sqrt(width * height)
  size <- max(min_size, min(max_size, size))
  size
}

# 
# auto_base_size <- function(width = 10,
#                            height = 6,
#                            min_size = 10,
#                            max_size = 16) {
#   
#   body <- list(
#     width = width,
#     height = height,
#     min_size = min_size,
#     max_size = max_size
#   )
#   
#   result <- .pos_api_post(
#     endpoint = "/auto_base_size",
#     body = body,
#     timeout_sec = 60
#   )
#   
#   as.numeric(result)
# }
