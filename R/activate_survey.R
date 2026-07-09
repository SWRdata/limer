#' activate_survey
#'
#' Sets the status of a survey to active
#'
#' @param iSurveyID integer, Id of the survey to be activated
#' @param verbose boolean, Giving out logging info
#'
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_activate_survey}
#' @example activate_survey("475835", verbose = TRUE)
#' @export

activate_survey <- function(iSurveyID, verbose = FALSE) {
  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()
  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!",
         call. = F)
  msg <- call_limer("activate_survey",
                    params = list("iSurveyID" = iSurveyID))

  if (msg$status == "OK") {
    if (verbose)
      message("Survey with id \u00b4",
              iSurveyID,
              "\u00b4 successfully activated!")

  } else if (msg == "No permission") {
    stop(
      glue::glue(
        "Either the survey with the ID \u00b4{iSurveyID}\u00b4 does not exist ",
        "or the permission to activate it is missing!"
      ),
      call. = F
    )
  } else {
    if (verbose)
      message(msg %>% unlist())
  }


}

