#' Get a LimeSurvey API session key
#'
#' This function logs into the LimeSurvey API and provides an access session key.
#' @param username LimeSurvey username. Defaults to value set in \code{options()}.
#' @param password LimeSurvey password Defaults to value set in \code{options()}.
#' @param ssl_verifypeer boolean \code{httr::config()} parameter. Default is
#' FALSE.
#' @return API token
#' @import httr
#' @export
#' @examples \dontrun{
#' get_session_key()
#' }
get_session_key <- function(
  username = getOption("lime_username"),
  password = getOption("lime_password"),
  lime_api = getOption("lime_api"),
  ssl_verifypeer = FALSE
) {
  if (is.null(lime_api)) {
    options(
      lime_api = "https://data.swr.de/medienanfrage/index.php/admin/remotecontrol"
    )
  }

  if (is.null(username)) {
    username <- Sys.getenv("LIME_USERNAME")
    options(lime_username = username)
  }

  if (is.null(password)) {
    password <- Sys.getenv("LIME_PASSWORD")
    options(lime_password = password)
  }

  body.json <- list(
    method = "get_session_key",
    id = 1,
    params = list(username, password)
  )

  r <- httr::POST(
    getOption("lime_api"),
    httr::content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE),
    httr::config(ssl_verifypeer = ssl_verifypeer),
    httr::timeout(60)
  )

  response_text <- httr::content(r, as = "text")
  session_key <- jsonlite::fromJSON(response_text)$result

  return(session_key)
}

# Start a new environment to hold the session key so all other functions can access it
# See http://trestletech.com/2013/04/package-wide-variablescache-in-r-package/
session_cache <- new.env(parent = emptyenv())
