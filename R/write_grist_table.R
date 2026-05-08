write_grist_table <- function(df,
                              table_name,
                              doc_id  = "r2HXjB5zhKA8LiuzxQYxmd",
                              server  = "https://grist.swr-datalab.de",
                              api_key = Sys.getenv("GRIST_API_KEY")) {
  
  url <- paste0(server, "/api/docs/", doc_id, "/tables/", table_name, "/records")
  
  if ("id" %in% colnames(df)) {
    records <- lapply(seq_len(nrow(df)), function(i) {
      list(
        id     = df$id[i],
        fields = as.list(df[i, setdiff(names(df), "id"), drop = FALSE])
      )
    })
    method <- httr::PATCH
  } else {
    records <- lapply(seq_len(nrow(df)), function(i) {
      list(fields = as.list(df[i, , drop = FALSE]))
    })
    method <- httr::POST
  }
  
  body <- jsonlite::toJSON(list(records = records), auto_unbox = TRUE)
  
  response <- method(
    url,
    httr::add_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = body
  )
  httr::stop_for_status(response)
  invisible(jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8")))
}
