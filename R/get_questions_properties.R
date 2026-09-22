#' get_questions_properties
#'
#' Generates a dataframe with the properties of all questions in a survey
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param sUsername character, Username in Limesurvey installation
#' @param verbose boolean, Giving out logging info
#'
#' @return dataframe
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_activate_survey}
#' @examples activate_survey("475835", verbose = TRUE)
#' @export

get_questions_properties <- function(iSurveyID,
                                     sUsername = NULL,
                                     verbose = FALSE) {
  if (!iSurveyID %in% limer::get_survey_list(sUsername = sUsername)) {
    stop(glue::glue("Survey with ID {iSurveyID} does not exist in this installation!"), call. = F)
  }

  res <- call_limer("list_questions", params = list("iSurveyID" = iSurveyID))
  return(res)
}
