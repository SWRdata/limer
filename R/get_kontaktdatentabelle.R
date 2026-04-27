#' Fetch a Table from Grist
#'
#' Retrieves records from a Grist document table and returns them as a
#' clean data frame with the \code{fields.} prefix removed from column names.
#'
#' @param table_name A character string specifying the Grist table name.
#'   The first letter needs to be capitalized.
#'   Defaults to \code{"Zentrale_datenbank"}.
#' @param doc_id A character string specifying the Grist document ID.
#'   Defaults to \code{"r2HXjB5zhKA8"}.
#' @param server A character string specifying the Grist server URL.
#'   Defaults to \code{"https://grist.swr-datalab.de"}.
#' @param api_key A character string containing the Grist API key.
#'   Defaults to \code{GRIST_API_KEY} from the calling environment.
#'
#' @return A data frame of records from the specified Grist table.

#' @importFrom httr GET add_headers stop_for_status content
#' @importFrom jsonlite fromJSON
#' @export
get_kontaktdatentabelle <- function(table_name = "Zentrale_datenbank",
                              doc_id     = "r2HXjB5zhKA8LiuzxQYxmd",
                              server     = "https://grist.swr-datalab.de",
                              api_key    = Sys.getenv("GRIST_API_KEY")) {

  url <- paste0(server, "/api/docs/", doc_id, "/tables/", table_name, "/records")

  response <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", api_key))
  )
  httr::stop_for_status(response)

  data <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    flatten = TRUE
  )

  df <- data$records
  names(df) <- gsub("^fields\\.", "", names(df))
  # rename name column because limesurvey expects firstname
  if("name" %in% colnames(df)){
    df |> dplyr::rename(firstname = name)
  }else if("gemeinde" %in% colnames(df)){
    df |> dplyr::rename(firstname = gemeinde)
  }


  return(df)
}
