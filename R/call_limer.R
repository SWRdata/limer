#' call_limer
#'
#' Low-level wrapper for making arbitrary JSON calls against a
#' LimeSurvey RemoteControl 2 API endpoint. This is the meta function that
#' most other `limer` functions (e.g. `get_survey_list()`,
#' `activate_survey()`, `add_responses()`) ultimately call under the hood.
#' A fresh session key is obtained automatically for every call, so callers
#' do not need to (and should not) pass a session key as part of `params`.
#'
#' @param method string, the RemoteControl API method name to call (e.g.
#' "list_surveys", "add_response", "activate_survey")
#' @param params list, the method's parameters, in the exact order
#' documented by the LimeSurvey API for that method (excluding the session
#' key, which is added automatically). Must be a list even for a single
#' parameter.
#' @param ssl_verifypeer boolean, whether to verify the SSL certificate of
#' the API host. Defaults to FALSE to accommodate self-signed certificates
#' common on internal LimeSurvey installations; set to TRUE for
#' public-facing instances where certificate validation is desired.
#' @param ... additional arguments passed on to \code{httr::POST}, e.g.
#'
#' @return The parsed \code{result} field of the JSON response.
#'
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html
#' @export
call_limer <- function(method, params = list(), ssl_verifypeer = FALSE, ...) {
  if (!is.list(params)) stop("params must be a list.")

  # Always get a fresh session key for every call
  fresh_key <- get_session_key()

  # LimeSurvey's methods expect *positional* arguments: the session
  # key always comes first, followed by the method's own parameters in
  # the order defined by the API docs. unname() strips any top-level
  # names from `params` (e.g. "iSurveyID") since JSON-RPC wants a plain
  # ordered array here, not a named object.
  params.full <- c(list(fresh_key), unname(params))
  body.json <- list(method = method, id = " ", params = params.full)

  r <- httr::POST(
    getOption("lime_api"),
    httr::content_type_json(),
    body = jsonlite::toJSON(body.json, auto_unbox = TRUE, force = TRUE),
    httr::config(ssl_verifypeer = ssl_verifypeer),
    ...
  )

  # simplifyVector = TRUE coerces JSON arrays/objects into R vectors,
  # lists, or data frames where possible, so most API responses come
  # back ready to use without further parsing
  response <- jsonlite::parse_json(
    httr::content(r, as = "text", encoding = "utf-8"),
    simplifyVector = TRUE
  )$result

  if (is.null(response)) {
    # A NULL `result` means the API returned an error object instead of
    # a result - re-parse the raw response (without simplifyVector) to
    # pull out the actual error message
    err_msg <- jsonlite::parse_json(
      httr::content(r, as = "text", encoding = "utf-8")
    )$error

    # If LimeSurvey didn't even recognise the method name, $error itself
    # may also come back NULL - fall back to a generic explanatory message
    # in that case rather than failing with a blank/uninformative error
    err_msg <- ifelse(is.null(err_msg),
                      glue::glue("{method} is an unknown function to remotecontrol"),
                      err_msg)
    stop(err_msg, call. = FALSE)
  } else {
    # Some API methods (e.g. list_questions) return a result where one
    # or more top-level columns are themselves nested data frames rather
    # than atomic values. Flatten these out with tidyr::unnest() so the
    # caller gets a single, regular data frame instead of a list-column
    # structure that's awkward to work with downstream.
    if (any(sapply(response, function(x) class(x)) == "data.frame")) {
      dataframe_columns <- sapply(response, function(x) is.data.frame(x))
      dataframe_column_names <- names(response)[dataframe_columns]
      response <- tidyr::unnest(response, cols = c(dataframe_column_names))
    }
    return(response)
  }
}
