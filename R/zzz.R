# R/zzz.R

.api_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # 优先读取环境变量，其次读取 options，最后用默认占位符
  url <- Sys.getenv("ONCOSEQOS_API_URL", unset = "")
  if (nchar(url) == 0) {
    url <- getOption("oncoseqos.api.base_url", default = "")
  }
  if (nchar(url) == 0) {
    url <- "http://localhost:10000"
  }
  .api_env$base_url <- url
}

#' Set API base URL
#' @param url The base URL of the API server
#' @export
set_api_url <- function(url) {
  .api_env$base_url <- url
  options(oncoseqos.api.base_url = url)
  message("API URL set to: ", url)
}

#' Get current API base URL
#' @export
get_api_url <- function() {
  .api_env$base_url
}
