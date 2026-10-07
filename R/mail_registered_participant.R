#' mail_registered_participant
#'
#' mail_registered_participant
#'
#' @param iSurveyID \dots
#' @param tid \dots
#' @export
#' @references \url{https://api.limesurvey.org/classes/remotecontrol-handle.html#method_mail_registered_participants}
#' @examples \dontrun{
#' mail_registred_participant(iSurveyID = 123456, tid = 2)
#' }
mail_registered_participant <- function(iSurveyID, tid) {

  call_limer(method = "mail_registered_participants", params = list("iSurveyID" = iSurveyID, "overrideAllConditions" = list("0" = paste0("tid = ", tid))))

}
