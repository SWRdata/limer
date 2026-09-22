#' delete_survey
#'
#' Deletes a survey and all its data
#'
#' @param iSurveyID integer, Id of the survey to be deleted
#' @param verbose boolean, Giving out logging info
#'
#' @references https://api.limesurvey.org/classes/Survey.html#method_deleteSurvey
#' @examples
#' \dontrun{
#' new_id <- copy_survey_to(iSurveyID = 475835, exclude_qids = c("G01Q03", "G01Q04"))
#' delete_survey(new_id)
#' }
#' @export

delete_survey <- function(iSurveyID, verbose = FALSE) {
  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()
  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!",
         call. = F)
  msg <- call_limer("delete_survey",
                    params = list("iSurveyID" = iSurveyID))

  if (msg == "No permission") {
    stop(
      glue::glue(
        "Either the survey with the ID \u00b4{iSurveyID}\u00b4 does not exist or the permission to delete it is missing!"
      ),
      call. = F
    )
  } else if (msg == "OK") {
    if (verbose)
      message("Survey with id \u00b4",
            iSurveyID,
            "\u00b4 successfully deleted!")
  } else {
    if (verbose)
      message(msg)
  }
}
