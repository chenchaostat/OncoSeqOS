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
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        size = base_size * 1.15,
        lineheight = 1.05,
        margin = ggplot2::margin(b = base_size * 0.4)
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = base_size * 0.95,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      axis.title = ggplot2::element_text(
        face = "bold",
        size = base_size * 1.05,
        color = "black"
      ),
      axis.text = ggplot2::element_text(
        size = base_size * 0.9,
        color = "black"
      ),
      axis.line = ggplot2::element_line(
        color = "black",
        linewidth = max(0.5, base_size / 18)
      ),
      axis.ticks = ggplot2::element_line(
        color = "black",
        linewidth = max(0.4, base_size / 22)
      ),
      axis.ticks.length = grid::unit(base_size * 0.22, "pt"),
      
      panel.border = ggplot2::element_blank(),
      panel.grid.major = if (grid) {
        ggplot2::element_line(color = "grey85", linewidth = max(0.25, base_size / 35))
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),
      
      legend.title = ggplot2::element_text(
        face = "bold",
        size = base_size * 0.95
      ),
      legend.text = ggplot2::element_text(
        size = base_size * 0.9
      ),
      legend.background = ggplot2::element_rect(
        fill = scales::alpha("white", 0.85),
        color = "grey55",
        linewidth = max(0.4, base_size / 25)
      ),
      legend.key = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      
      plot.margin = ggplot2::margin(
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
