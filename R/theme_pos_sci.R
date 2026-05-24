#' @title Scientific ggplot2 Theme Through API
#' @description
#' Generate a scientific-style ggplot2 theme.
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
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = base_size * 1.15,
        lineheight = 1.05,
        margin = margin(b = base_size * 0.4)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = base_size * 0.95,
        margin = margin(b = base_size * 0.5)
      ),
      axis.title = element_text(
        face = "bold",
        size = base_size * 1.05,
        color = "black"
      ),
      axis.text = element_text(
        size = base_size * 0.9,
        color = "black"
      ),
      axis.line = element_line(
        color = "black",
        linewidth = max(0.5, base_size / 18)
      ),
      axis.ticks = element_line(
        color = "black",
        linewidth = max(0.4, base_size / 22)
      ),
      axis.ticks.length = unit(base_size * 0.22, "pt"),
      
      panel.border = element_blank(),
      panel.grid.major = if (grid) {
        element_line(color = "grey85", linewidth = max(0.25, base_size / 35))
      } else {
        element_blank()
      },
      panel.grid.minor = element_blank(),
      
      legend.title = element_text(
        face = "bold",
        size = base_size * 0.95
      ),
      legend.text = element_text(
        size = base_size * 0.9
      ),
      legend.background = element_rect(
        fill = alpha("white", 0.85),
        color = "grey55",
        linewidth = max(0.4, base_size / 25)
      ),
      legend.key = element_blank(),
      legend.box.background = element_blank(),
      
      plot.margin = margin(
        t = base_size * 0.8,
        r = base_size * 0.8,
        b = base_size * 0.8,
        l = base_size * 0.8
      )
    )
}





# theme_pos_sci <- function(
#     base_size = 12,
#     base_family = "sans",
#     grid = TRUE
# ) {
#   
#   body <- list(
#     base_size = base_size,
#     base_family = base_family,
#     grid = grid
#   )
#   
#   result <- .pos_api_post(
#     endpoint = "/theme_pos_sci",
#     body = body,
#     timeout_sec = 60
#   )
#   
#   if (is.null(result$theme_rds_base64)) {
#     stop(
#       "API response does not contain `theme_rds_base64`. ",
#       "Please check the server endpoint `/theme_pos_sci`.",
#       call. = FALSE
#     )
#   }
#   
#   raw_theme <- jsonlite::base64_dec(result$theme_rds_base64)
#   
#   con <- rawConnection(raw_theme, open = "rb")
#   on.exit(close(con), add = TRUE)
#   
#   theme_obj <- unserialize(con)
#   
#   theme_obj
# }
