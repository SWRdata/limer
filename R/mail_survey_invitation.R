#' mail_survey_invitation
#'
#' Send invitation emails to one or more survey participants via the LimeSurvey
#' RemoteControl API method \code{invite_participants}.
#'
#' Participants must have a non-empty access \code{token}. Create tokens with
#' \code{add_participants(..., bCreateToken = TRUE)} or afterwards with
#' \code{add_participant_codes(iSurveyID)}.
#'
#' @param iSurveyID integer, ID of the survey whose participants should be
#' invited
#' @param tid integer, one or more token IDs (participant table row IDs) to
#' invite. A scalar or a vector is accepted.
#' @param bEmail boolean, if \code{TRUE} (default) send only pending invites
#' (\code{sent} is \code{N}/empty); if \code{FALSE} resend invites only
#' (participant must already have been invited)
#' @param continueOnError boolean, if \code{TRUE} do not stop on the first
#' invalid participant (default \code{FALSE})
#'
#' @return array of results of sending
#' @references \url{https://api.limesurvey.org/classes/remotecontrol-handle.html#method_invite_participants}
#' @examples
#' \dontrun{
#' mail_survey_invitation(iSurveyID = 475835, tid = 2)
#' invite_participants(475835, tid = c(2, 5, 9))
#' }
#' @export
mail_survey_invitation <- function(iSurveyID, tid,
                                   bEmail = TRUE,
                                   continueOnError = FALSE) {
  resp <- call_limer(
    method = "invite_participants",
    params = list(
      iSurveyID = iSurveyID,
      aTokenIds = as.list(tid),
      bEmail = bEmail,
      continueOnError = continueOnError
    )
  )

  # LimeSurvey found no tokens matching its invite filters
  if (is.list(resp) && !is.null(resp$status) &&
      identical(resp$status, "Error: No candidate tokens")) {
    state <- fetch_participant_mail_state(iSurveyID, tid)
    details <- format_invite_ineligibility(state, bEmail)
    msg <- glue::glue(
      "No eligible tokens for invitation ",
      "(tid: {paste(tid, collapse = ', ')}).\n{details}"
    )
    message(msg)
    return(invisible(as.character(msg)))
  }

  resp
}


#' @rdname mail_survey_invitation
#' @export
invite_participants <- mail_survey_invitation
