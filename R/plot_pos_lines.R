#' @title Plot POS Lines
#'
#' @description
#' Generate a line plot for probability of success or related summary metrics.
#'
#' @param data A data.frame containing plotting data.
#' @param x Character string. Column name used as x-axis.
#' @param y_cols Character vector. Column names used as y-axis variables.
#' @param labels Optional character vector of labels for `y_cols`.
#' @param colors Optional named or unnamed color vector.
#' @param title Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param y_transform Function used to transform y values. Default is `identity`.
#' @param x_transform Function used to transform x values. Default transforms proportions to percentages.
#' @param y_limits Optional numeric vector of length 2.
#' @param y_breaks Optional y-axis breaks. Use `ggplot2::waiver()` to let ggplot2 decide.
#' @param x_breaks_n Number of x-axis breaks.
#' @param y_breaks_n Number of y-axis breaks.
#' @param y_label_accuracy Optional y-axis label accuracy.
#' @param x_label_accuracy X-axis label accuracy.
#' @param legend_title Legend title.
#' @param legend_position Legend position. Can be `"auto"`, `"right"`, `"bottom"`, `"none"`,
#'   or a numeric vector of length 2.
#' @param width Plot width.
#' @param height Plot height.
#' @param base_size Base font size.
#' @param line_width Line width.
#' @param point_size Point size.
#' @param title_width Title wrap width.
#' @param include_zero Logical. Whether y-axis limits should include zero.
#' @param grid Logical. Whether to show grid lines.
#'
#' @return A ggplot object.
#'
#' @export


plot_pos_lines <- function(
    data,
    x,
    y_cols,
    labels = NULL,
    colors = NULL,
    title = NULL,
    xlab = NULL,
    ylab = NULL,
    y_transform = identity,
    x_transform = function(z) z * 100,
    y_limits = NULL,
    y_breaks = ggplot2::waiver(),
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
    grid = TRUE
) {
  stopifnot(is.data.frame(data))
  
  if (!x %in% names(data)) {
    stop("Column specified in `x` not found: ", x)
  }
  
  missing_y <- setdiff(y_cols, names(data))
  if (length(missing_y) > 0) {
    stop("The following y columns are missing: ", paste(missing_y, collapse = ", "))
  }
  
  if (is.null(base_size)) {
    base_size <- auto_base_size(width = width, height = height)
  }
  
  if (is.null(line_width)) {
    line_width <- max(0.8, base_size / 11)
  }
  
  if (is.null(point_size)) {
    point_size <- max(1.8, base_size / 4.5)
  }
  
  if (is.null(labels)) {
    labels <- y_cols
  }
  
  if (length(labels) != length(y_cols)) {
    stop("`labels` must have the same length as `y_cols`.")
  }
  
  plot_dat <- data |>
    dplyr::arrange(rlang::.data[[x]]) |>
    dplyr::mutate(
      .x_plot = x_transform(rlang::.data[[x]])
    ) |>
    dplyr::select(rlang::.data$.x_plot, dplyr::all_of(y_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(y_cols),
      names_to = ".metric",
      values_to = ".value"
    ) |>
    dplyr::mutate(
      .metric = factor(rlang::.data$.metric, levels = y_cols, labels = labels),
      .value = y_transform(rlang::.data$.value)
    )
  
  y_lim <- auto_limits(
    plot_dat$.value,
    hard_limits = y_limits,
    include_zero = include_zero
  )
  
  if (identical(y_breaks, ggplot2::waiver())) {
    y_breaks <- scales::breaks_extended(n = y_breaks_n)
  }
  
  if (is.null(y_label_accuracy)) {
    y_labels <- ggplot2::waiver()
  } else {
    y_labels <- scales::label_number(accuracy = y_label_accuracy)
  }
  
  if (is.null(colors)) {
    colors <- scales::hue_pal()(length(labels))
    names(colors) <- labels
  } else {
    if (is.null(names(colors))) {
      names(colors) <- labels
    }
  }
  
  if (!is.null(title)) {
    title <- stringr::str_wrap(title, width = title_width)
  }
  
  p <- ggplot2::ggplot(
    plot_dat,
    ggplot2::aes(
      x = rlang::.data$.x_plot,
      y = rlang::.data$.value,
      color = rlang::.data$.metric,
      group = rlang::.data$.metric
    )
  ) +
    ggplot2::geom_line(linewidth = line_width, na.rm = TRUE) +
    ggplot2::geom_point(size = point_size, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = colors, drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(n = x_breaks_n),
      labels = scales::label_number(accuracy = x_label_accuracy),
      expand = ggplot2::expansion(mult = c(0.02, 0.04))
    ) +
    ggplot2::scale_y_continuous(
      limits = y_lim,
      breaks = y_breaks,
      labels = y_labels,
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = legend_title
    ) +
    theme_pos_sci(
      base_size = base_size,
      grid = grid
    )
  
  if (identical(legend_position, "auto")) {
    lp <- auto_legend_position(
      plot_dat,
      x = ".x_plot",
      y = ".value",
      prefer = "auto"
    )
    
    if (is.numeric(lp)) {
      p <- p +
        ggplot2::theme(
          legend.position = lp,
          legend.justification = if (lp[1] < 0.5) c(0, 1) else c(1, 1)
        )
    } else {
      p <- p + ggplot2::theme(legend.position = lp)
    }
    
  } else if (is.numeric(legend_position)) {
    p <- p +
      ggplot2::theme(
        legend.position = legend_position,
        legend.justification = if (legend_position[1] < 0.5) c(0, 1) else c(1, 1)
      )
  } else {
    p <- p + ggplot2::theme(legend.position = legend_position)
  }
  
  p
}



# plot_pos_lines <- function(
#     data,
#     x,
#     y_cols,
#     labels = NULL,
#     colors = NULL,
#     title = NULL,
#     xlab = NULL,
#     ylab = NULL,
#     
#     # API-friendly versions of transformation arguments.
#     # The server should map these strings to actual R functions.
#     y_transform = "identity",
#     x_transform = "percent",
#     
#     y_limits = NULL,
#     y_breaks = NULL,
#     x_breaks_n = 8,
#     y_breaks_n = 6,
#     y_label_accuracy = NULL,
#     x_label_accuracy = 1,
#     legend_title = NULL,
#     legend_position = "auto",
#     width = 10,
#     height = 6,
#     base_size = NULL,
#     line_width = NULL,
#     point_size = NULL,
#     title_width = 68,
#     include_zero = FALSE,
#     grid = TRUE,
#     
#     return_type = "plot",
#     timeout_sec = 3600
# ) {
#   
#   stopifnot(is.data.frame(data))
#   
#   body <- list(
#     data = data,
#     x = x,
#     y_cols = y_cols,
#     
#     labels = labels,
#     colors = colors,
#     title = title,
#     xlab = xlab,
#     ylab = ylab,
#     
#     y_transform = y_transform,
#     x_transform = x_transform,
#     
#     y_limits = y_limits,
#     y_breaks = y_breaks,
#     x_breaks_n = x_breaks_n,
#     y_breaks_n = y_breaks_n,
#     y_label_accuracy = y_label_accuracy,
#     x_label_accuracy = x_label_accuracy,
#     
#     legend_title = legend_title,
#     legend_position = legend_position,
#     
#     width = width,
#     height = height,
#     base_size = base_size,
#     line_width = line_width,
#     point_size = point_size,
#     title_width = title_width,
#     
#     include_zero = include_zero,
#     grid = grid,
#     
#     return_type = return_type
#   )
#   
#   result <- .pos_api_post(
#     endpoint = "/plot_pos_lines",
#     body = body,
#     timeout_sec = timeout_sec
#   )
#   
#   result
# }
