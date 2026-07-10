#' Get a participant property from a LimeSurvey survey
#'
#' This function exports and downloads a participant property from a LimeSurvey
#' survey.
#' @param iSurveyID \dots
#' @param aTokenQueryProperties \dots
#' @param aTokenProperties \dots
#' @export
#' @examples
#' \dontrun{
#' get_participant_property(iSurveyID = 475835,
#'                                    aTokenQueryProperties = 1)
#' }
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_activate_survey}

get_participant_property <- function(iSurveyID,
                                     aTokenQueryProperties,
                                     aTokenProperties = NULL) {
  params <- as.list(environment())
  result <- call_limer(method = "get_participant_properties", params = params)
  return(result)
}