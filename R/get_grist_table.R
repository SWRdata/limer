get_grist_table <- function(table_name,
                            doc_id  = "",
                            server  = "https://grist.swr-datalab.de",
                            api_key = Sys.getenv("GRIST_API_KEY")) {
  url <- paste0(server, "/api/docs/", doc_id, "/tables/", table_name, "/records")
  response <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key)))
  httr::stop_for_status(response)
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
  df <- data$records
  names(df) <- gsub("^fields\\.", "", names(df))
  df
}
