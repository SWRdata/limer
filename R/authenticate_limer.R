#' authenticate_limer
#'
#' Authenticates a user session to a Limesurvey instance. The function
#' requires that the parameters LIME_USERNAME, LIME_PASSOWRD and
#' LIME_API_URL are set in the system environment in .Renviron.
#'
#' @param ssl_verifypeer boolean, httr::config parameter for API call
#' @return message
#' @examples
#' \dontrun{
#' authenticate_limer()
#' }
#' @export
#'

authenticate_limer <- function(ssl_verifypeer = FALSE){

  options(lime_username = Sys.getenv("LIME_USERNAME"))
  options(lime_password = Sys.getenv("LIME_PASSWORD"))
  options(lime_api = Sys.getenv("LIME_API_URL"))

  if (getOption("lime_api") == "")
    stop("The parameter LIME_API_URL is not set in the system environment!", call. = FALSE)

  get_session_key(ssl_verifypeer = ssl_verifypeer)

}

