#' Get a participant property from a LimeSurvey survey
#'
#' This function exports and downloads a participant property from a LimeSurvey
#' survey.
#' @param iSurveyID ID of the Survey to get token properties
#' @param aTokenQueryProperties token id (Tid) as an integer
#' @param aTokenProperties The properties to get
#' @export
#' @examples
#' \dontrun{
#' get_participant_property(
#'   iSurveyID = 475835,
#'   aTokenQueryProperties = 1
#' )
#' }
#' @references \url{https://api.limesurvey.org/classes/remotecontrol-handle.html#method_get_participant_properties}

get_participant_property <- function(iSurveyID,
                                     aTokenQueryProperties,
                                     aTokenProperties = NULL) {
  params <- as.list(environment())
  result <- call_limer(method = "get_participant_properties", params = params)
  return(result)
}
