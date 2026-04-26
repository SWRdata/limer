call_limer <- function(method, params = list(), ssl_verifypeer = FALSE, ...) {
  if (!is.list(params)) stop("params must be a list.")

  # Always get a fresh session key for every call
  fresh_key <- get_session_key()

  params.full <- c(list(fresh_key), unname(params))
  body.json <- list(method = method, id = " ", params = params.full)

  r <- httr::POST(
    getOption("lime_api"),
    httr::content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE, force = TRUE),
    httr::config(ssl_verifypeer = ssl_verifypeer),
    ...
  )

  response <- jsonlite::parse_json(
    httr::content(r, as = "text", encoding = "utf-8"),
    simplifyVector = TRUE
  )$result

  if (is.null(response)) {
    err_msg <- jsonlite::parse_json(
      httr::content(r, as = "text", encoding = "utf-8")
    )$error
    err_msg <- ifelse(is.null(err_msg),
                      glue::glue("{method} is an unknown function to remotecontrol"),
                      err_msg)
    stop(err_msg, call. = FALSE)
  } else {
    if (any(sapply(response, function(x) class(x)) == "data.frame")) {
      dataframe_columns <- sapply(response, function(x) is.data.frame(x))
      dataframe_column_names <- names(response)[dataframe_columns]
      response <- tidyr::unnest(response, cols = c(dataframe_column_names))
    }
    return(response)
  }
}