.pos_api_post <- function(endpoint, body = list(), timeout_sec = 600) {
  
  base_url <- getOption(
    "pos.api.base_url",
    default = "https://pos-api-server.onrender.com"
  )
  
  url <- paste0(base_url, endpoint)
  
  response <- tryCatch({
    httr::POST(
      url = url,
      body = jsonlite::toJSON(
        body,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        dataframe = "rows"
      ),
      httr::content_type_json(),
      httr::timeout(timeout_sec),
      encode = "raw"
    )
  }, error = function(e) {
    stop(
      "Unable to connect to the API server. Please check your internet connection.\n",
      "Endpoint: ", endpoint, "\n",
      "Error: ", e$message,
      call. = FALSE
    )
  })
  
  status <- httr::status_code(response)
  
  result_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  if (status != 200) {
    stop(
      "API request failed.\n",
      "Endpoint: ", endpoint, "\n",
      "Status code: ", status, "\n",
      "Response: ", result_text,
      call. = FALSE
    )
  }
  
  jsonlite::fromJSON(
    result_text,
    simplifyDataFrame = TRUE,
    simplifyVector = TRUE
  )
}
