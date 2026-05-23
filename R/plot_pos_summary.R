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
  
  body <- list(
    summary_data = summary_data,
    x = x,
    therapy_name = therapy_name,
    width = width,
    height = height,
    base_size = base_size,
    return = return,
    title_prefix = title_prefix
  )
  
  result <- .pos_api_post(
    endpoint = "/plot_pos_summary",
    body = body,
    timeout_sec = 600
  )
  
  result
}
