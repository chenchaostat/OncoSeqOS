#' @title Automatically Determine Legend Position Through API
#' @description
#' Calls server API to automatically determine the best legend position
#' based on the distribution of x and y values in the input data.
#'
#' @param data A data.frame containing x and y variables.
#' @param x Character string. Name of the x variable in `data`.
#' @param y Character string. Name of the y variable in `data`.
#' @param prefer Preferred legend position. One of `"auto"`, `"left"`, `"right"`, `"bottom"`, `"none"`.
#'
#' @returns
#' A numeric vector such as `c(0.02, 0.98)` or `c(0.98, 0.98)`,
#' or a character value such as `"bottom"` or `"none"`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   time = 1:10,
#'   surv = c(0.98, 0.95, 0.91, 0.86, 0.80, 0.74, 0.69, 0.62, 0.55, 0.48)
#' )
#'
#' auto_legend_position(
#'   data = df,
#'   x = "time",
#'   y = "surv",
#'   prefer = "auto"
#' )
#' }



auto_legend_position <- function(data, x, y, prefer = c("auto", "left", "right", "bottom", "none")) {
  prefer <- match.arg(prefer)
  
  if (prefer == "left") {
    return(c(0.02, 0.98))
  }
  
  if (prefer == "right") {
    return(c(0.98, 0.98))
  }
  
  if (prefer == "bottom") {
    return("bottom")
  }
  
  if (prefer == "none") {
    return("none")
  }
  
  x_val <- data[[x]]
  y_val <- data[[y]]
  
  x_mid <- stats::median(x_val, na.rm = TRUE)
  y_mid <- stats::median(y_val, na.rm = TRUE)
  
  upper_right_density <- mean(x_val > x_mid & y_val > y_mid, na.rm = TRUE)
  upper_left_density  <- mean(x_val < x_mid & y_val > y_mid, na.rm = TRUE)
  
  if (upper_right_density > upper_left_density) {
    c(0.02, 0.98)
  } else {
    c(0.98, 0.98)
  }
}


# 
# auto_legend_position <- function(
#     data,
#     x,
#     y,
#     prefer = c("auto", "left", "right", "bottom", "none")
# ) {
#   
#   prefer <- match.arg(prefer)
#   
#   if (!is.data.frame(data)) {
#     stop("`data` must be a data.frame.", call. = FALSE)
#   }
#   
#   if (!is.character(x) || length(x) != 1) {
#     stop("`x` must be a single character string.", call. = FALSE)
#   }
#   
#   if (!is.character(y) || length(y) != 1) {
#     stop("`y` must be a single character string.", call. = FALSE)
#   }
#   
#   if (!x %in% names(data)) {
#     stop("Column `", x, "` not found in `data`.", call. = FALSE)
#   }
#   
#   if (!y %in% names(data)) {
#     stop("Column `", y, "` not found in `data`.", call. = FALSE)
#   }
#   
#   body <- list(
#     data = data,
#     x = x,
#     y = y,
#     prefer = prefer
#   )
#   
#   result <- .pos_api_post(
#     endpoint = "/auto_legend_position",
#     body = body,
#     timeout_sec = 60
#   )
#   
#   result
# }
