#' @title Scientific ggplot2 Theme Through API
#' @description
#' Calls server API to generate a scientific-style ggplot2 theme.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param grid Logical. Whether to show major grid lines.
#'
#' @returns A ggplot2 theme object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_pos_sci()
#' }
theme_pos_sci <- function(
    base_size = 12,
    base_family = "sans",
    grid = TRUE
) {
  
  body <- list(
    base_size = base_size,
    base_family = base_family,
    grid = grid
  )
  
  result <- .pos_api_post(
    endpoint = "/theme_pos_sci",
    body = body,
    timeout_sec = 60
  )
  
  if (is.null(result$theme_rds_base64)) {
    stop(
      "API response does not contain `theme_rds_base64`. ",
      "Please check the server endpoint `/theme_pos_sci`.",
      call. = FALSE
    )
  }
  
  raw_theme <- jsonlite::base64_dec(result$theme_rds_base64)
  
  con <- rawConnection(raw_theme, open = "rb")
  on.exit(close(con), add = TRUE)
  
  theme_obj <- unserialize(con)
  
  theme_obj
}
