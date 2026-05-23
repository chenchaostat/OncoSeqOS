#' @title Plot POS Lines Through API
#' @description
#' Calls server API to generate POS line plot.
#'
#' @param data A data.frame containing plotting data.
#' @param x Column name used as x-axis.
#' @param y_cols Character vector of y-axis column names.
#' @param labels Optional labels for y columns.
#' @param colors Optional named or unnamed color vector.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param y_transform Y transformation name. For example: "identity", "percent".
#' @param x_transform X transformation name. For example: "identity", "percent".
#' @param y_limits Optional numeric vector of length 2.
#' @param y_breaks Optional y breaks. Use NULL to let server decide automatically.
#' @param x_breaks_n Number of x-axis breaks.
#' @param y_breaks_n Number of y-axis breaks.
#' @param y_label_accuracy Optional y-axis label accuracy.
#' @param x_label_accuracy X-axis label accuracy.
#' @param legend_title Legend title.
#' @param legend_position Legend position. Can be "auto", "right", "bottom", etc.,
#'   or a numeric vector of length 2.
#' @param width Plot width.
#' @param height Plot height.
#' @param base_size Base font size.
#' @param line_width Line width.
#' @param point_size Point size.
#' @param title_width Title wrap width.
#' @param include_zero Whether y-axis limits should include zero.
#' @param grid Whether to show grid.
#' @param return_type Type of result returned by API. For example: "plot", "png", "svg".
#' @param timeout_sec API timeout in seconds.
#'
#' @returns API response containing plot result or plot metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- plot_pos_lines(
#'   data = res$summary,
#'   x = "prop_ctl_subseq2",
#'   y_cols = c("pos_interim", "pos_final"),
#'   labels = c("Interim POS", "Final POS"),
#'   xlab = "Control subsequent therapy 2 proportion (%)",
#'   ylab = "POS",
#'   title = "Probability of Success by Subsequent Therapy Proportion"
#' )
#' }
plot_pos_lines <- function(
    data,
    x,
    y_cols,
    labels = NULL,
    colors = NULL,
    title = NULL,
    xlab = NULL,
    ylab = NULL,
    
    # API-friendly versions of transformation arguments.
    # The server should map these strings to actual R functions.
    y_transform = "identity",
    x_transform = "percent",
    
    y_limits = NULL,
    y_breaks = NULL,
    x_breaks_n = 8,
    y_breaks_n = 6,
    y_label_accuracy = NULL,
    x_label_accuracy = 1,
    legend_title = NULL,
    legend_position = "auto",
    width = 10,
    height = 6,
    base_size = NULL,
    line_width = NULL,
    point_size = NULL,
    title_width = 68,
    include_zero = FALSE,
    grid = TRUE,
    
    return_type = "plot",
    timeout_sec = 3600
) {
  
  stopifnot(is.data.frame(data))
  
  body <- list(
    data = data,
    x = x,
    y_cols = y_cols,
    
    labels = labels,
    colors = colors,
    title = title,
    xlab = xlab,
    ylab = ylab,
    
    y_transform = y_transform,
    x_transform = x_transform,
    
    y_limits = y_limits,
    y_breaks = y_breaks,
    x_breaks_n = x_breaks_n,
    y_breaks_n = y_breaks_n,
    y_label_accuracy = y_label_accuracy,
    x_label_accuracy = x_label_accuracy,
    
    legend_title = legend_title,
    legend_position = legend_position,
    
    width = width,
    height = height,
    base_size = base_size,
    line_width = line_width,
    point_size = point_size,
    title_width = title_width,
    
    include_zero = include_zero,
    grid = grid,
    
    return_type = return_type
  )
  
  result <- .pos_api_post(
    endpoint = "/plot_pos_lines",
    body = body,
    timeout_sec = timeout_sec
  )
  
  result
}
