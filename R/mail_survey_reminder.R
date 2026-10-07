#' mail_survey_reminder
#'
#' Send reminder emails to one or more survey participants via the LimeSurvey
#' RemoteControl API method \code{remind_participants}.
#'
#' Optional integer arguments \code{iMinDaysBetween} and \code{iMaxReminders}
#' are always passed (as JSON \code{null} when unset) so that \code{aTokenIds}
#' stays in the correct positional slot after \code{call_limer()} unnames
#' the parameter list.
#'
#' @param iSurveyID integer, ID of the survey whose participants should be
#' reminded
#' @param tid integer, one or more token IDs (participant table row IDs) to
#' remind. A scalar or a vector is accepted.
#' @param iMinDaysBetween integer or \code{NULL}, minimum days since the last
#' reminder (optional; default \code{NULL})
#' @param iMaxReminders integer or \code{NULL}, maximum number of reminders
#' already sent (optional; default \code{NULL})
#' @param continueOnError boolean, if \code{TRUE} do not stop on the first
#' invalid participant (default \code{FALSE})
#'
#' @return array of results of sending
#' @references \url{https://api.limesurvey.org/classes/remotecontrol-handle.html#method_remind_participants}
#' @examples
#' \dontrun{
#' mail_survey_reminder(iSurveyID = 475835, tid = 2)
#' remind_participants(475835, tid = c(2, 5))
#' }
#' @export
mail_survey_reminder <- function(iSurveyID, tid,
                                 iMinDaysBetween = NULL,
                                 iMaxReminders = NULL,
                                 continueOnError = FALSE) {
  resp <- call_limer(
    method = "remind_participants",
    params = list(
      iSurveyID = iSurveyID,
      iMinDaysBetween = iMinDaysBetween,
      iMaxReminders = iMaxReminders,
      aTokenIds = as.list(tid),
      continueOnError = continueOnError
    )
  )

  # LimeSurvey found no tokens matching its reminder filters
  if (is.list(resp) && !is.null(resp$status) &&
      identical(resp$status, "Error: No candidate tokens")) {
    state <- fetch_participant_mail_state(iSurveyID, tid)
    details <- format_reminder_ineligibility(
      state,
      iMinDaysBetween = iMinDaysBetween,
      iMaxReminders = iMaxReminders
    )
    msg <- glue::glue(
      "No eligible tokens for reminder ",
      "(tid: {paste(tid, collapse = ', ')}).\n{details}"
    )
    message(msg)
    return(invisible(as.character(msg)))
  }

  resp
}


#' @rdname mail_survey_reminder
#' @export
remind_participants <- mail_survey_reminder
