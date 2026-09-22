#' exists_participants_table
#'
#' checks if a participant table already exists
#'
#' @param iSurveyID integer, ID of the Survey
#'
#' @return boolean
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_activate_survey}
#' @examples
#' \dontrun{
#' exists_participants_table(475835)
#' }
#' @export
#'

exists_participants_table <- function(iSurveyID) {
  params <- list("iSurveyID" = iSurveyID,"bUnused" = TRUE, "iLimit" = 1, "iStart" = 1 )
  resp <- call_limer(method = "list_participants", params = params)
  if (!"status" %in% names(resp) || resp$status == "No survey participants found.") {
    TRUE
  } else {
    FALSE
  }
}