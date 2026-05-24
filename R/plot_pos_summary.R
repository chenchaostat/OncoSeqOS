#' @title Visualization for PoS summary results.
#' @param summary_data The input summary dataset used to generate the plots. It must contain the required columns such as probability metrics, survival rates, median survival, and hazard ratio.
#' @param x The column name used as the x-axis variable. In this function, it usually represents the proportion of control-arm patients receiving subsequent therapy after disease progression.
#' @param therapy_name The name of the subsequent therapy displayed in the plot titles and x-axis labels.
#' @param width The intended plot width. It is mainly used to automatically determine the base font size when base_size = NULL.
#' @param height The intended plot height. It is also used to automatically determine the base font size when base_size = NULL.
#' @param base_size The base font size for all plots. If NULL, the function automatically calculates a suitable font size using auto_base_size(width, height).
#' @param return Determines the output format. If "list", the function returns a named list of four separate ggplot objects. If "patchwork", the function combines the four plots vertically using the patchwork package.
#' @param title_prefix A custom prefix added to each plot title. If NULL, the function automatically creates a title prefix based on therapy_name.
#' @export


plot_pos_summary <- function(
    summary_data,
    x = "prop_ctl_subseq2",
    therapy_name = "CD20/CD30",
    width = 10,
    height = 6,
    base_size = NULL,
    return = c("list", "patchwork"),
    title_prefix = NULL
) {
  return <- match.arg(return)
  
  required_cols <- c(
    x,
    "prob_hr_lt", "POS", "final_CondPOS", "interim_POS",
    "mean_SurvRate12C_final", "mean_SurvRate12T_final",
    "mean_medSurvC_final", "mean_medSurvT_final",
    "mean_hr_final"
  )
  
  missing_cols <- setdiff(required_cols, names(summary_data))
  
  if (length(missing_cols) > 0) {
    stop(
      "The following required columns are missing in summary_data: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (is.null(base_size)) {
    base_size <- auto_base_size(width = width, height = height)
  }
  
  if (is.null(title_prefix)) {
    title_prefix <- paste0(
      "Effect of proportion of control patients receiving ",
      therapy_name,
      " after PD on "
    )
  }
  
  xlab <- paste0(
    "Proportion of control patients receiving ",
    therapy_name,
    " after PD (%)"
  )
  
  cols_prob <- c(
    "Pr(HR < 0.75)" = "#1B9E77",
    "Overall Power" = "#D95F02",
    "Final Conditional Power" = "#7570B3",
    "Interim Power" = "#E7298A"
  )
  
  cols_group <- c(
    "Control arm" = "#1B9E77",
    "Treatment arm" = "#D95F02"
  )
  
  col_hr <- c("Mean hazard ratio" = "#2C7FB8")
  
  dat <- summary_data %>%
    arrange(.data[[x]])
  
  p1 <- plot_pos_lines(
    data = dat,
    x = x,
    y_cols = c("prob_hr_lt", "POS", "final_CondPOS", "interim_POS"),
    labels = c("Pr(HR < 0.75)", "Overall Power", "Final Conditional Power", "Interim Power"),
    colors = cols_prob,
    title = paste0(title_prefix, "probability-related metrics"),
    xlab = xlab,
    ylab = "Probability",
    y_limits = c(0, 1),
    y_breaks = seq(0, 1, by = 0.1),
    y_label_accuracy = 0.01,
    legend_title = "Metric",
    legend_position = c(0.98, 0.98),
    width = width,
    height = height,
    base_size = base_size,
    include_zero = TRUE
  )
  
  p2 <- plot_pos_lines(
    data = dat,
    x = x,
    y_cols = c("mean_SurvRate12C_final", "mean_SurvRate12T_final"),
    labels = c("Control arm", "Treatment arm"),
    colors = cols_group,
    title = paste0(title_prefix, "12-month survival rate"),
    xlab = xlab,
    ylab = "12-month survival rate (%)",
    y_transform = function(z) z * 100,
    y_breaks_n = 6,
    y_label_accuracy = 1,
    legend_title = "Group",
    legend_position = c(0.02, 0.98),
    width = width,
    height = height,
    base_size = base_size
  )
  
  p3 <- plot_pos_lines(
    data = dat,
    x = x,
    y_cols = c("mean_medSurvC_final", "mean_medSurvT_final"),
    labels = c("Control arm", "Treatment arm"),
    colors = cols_group,
    title = paste0(title_prefix, "median overall survival"),
    xlab = xlab,
    ylab = "Median overall survival (months)",
    y_breaks_n = 6,
    y_label_accuracy = 0.1,
    legend_title = "Group",
    legend_position = c(0.02, 0.98),
    width = width,
    height = height,
    base_size = base_size
  )
  
  p4 <- plot_pos_lines(
    data = dat %>% mutate(.mean_hr_label = mean_hr_final),
    x = x,
    y_cols = ".mean_hr_label",
    labels = "Mean hazard ratio",
    colors = col_hr,
    title = paste0(title_prefix, "mean hazard ratio"),
    xlab = xlab,
    ylab = "Mean hazard ratio",
    y_breaks_n = 7,
    y_label_accuracy = 0.001,
    legend_title = NULL,
    legend_position = "none",
    width = width,
    height = height,
    base_size = base_size
  )
  
  plots <- list(
    probability = p1,
    survival_rate_12m = p2,
    median_survival = p3,
    mean_hr = p4
  )
  
  if (return == "patchwork") {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package `patchwork` is required when return = 'patchwork'.")
    }
    
    return(
      (p1 / p2 / p3 / p4) +
        patchwork::plot_layout(heights = c(1, 1, 1, 1))
    )
  }
  
  plots
}



# 
# plot_pos_summary <- function(
#     summary_data,
#     x = "prop_ctl_subseq2",
#     therapy_name = "CD20/CD30",
#     width = 10,
#     height = 6,
#     base_size = NULL,
#     return = c("list", "patchwork"),
#     title_prefix = NULL
# ) {
#   
#   return <- match.arg(return)
#   
#   body <- list(
#     summary_data = summary_data,
#     x = x,
#     therapy_name = therapy_name,
#     width = width,
#     height = height,
#     base_size = base_size,
#     return = return,
#     title_prefix = title_prefix
#   )
#   
#   result <- .pos_api_post(
#     endpoint = "/plot_pos_summary",
#     body = body,
#     timeout_sec = 600
#   )
#   
#   result
# }
