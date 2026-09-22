#' Release a LimeSurvey API session key
#'
#' This function clears the LimeSurvey API session key currently in use, effectively logging out.
#'
#' @export
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_activate_survey}
#' @examples \dontrun{
#' release_session_key()
#' }

release_session_key <- function() {
  call_limer(method = "release_session_key")
}
