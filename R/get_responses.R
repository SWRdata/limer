#' Export data from a LimeSurvey survey
#'
#' This function exports and downloads data from a LimeSurvey survey.
#' @param iSurveyID string ID of the Survey
#' @param sDocumentType string any format available by plugins (for example :
#' pdf, csv, xls, doc, json).
#' @param sLanguageCode string (optional) The language to be used.
#' @param sCompletionStatus string (optional) 'complete','incomplete' or 'all'
#' - defaults to 'all'.
#' @param sHeadingType string (optional) 'code','full' or 'abbreviated'.
#' defaults to 'code'.
#' @param sResponseType (optional)'short' or 'long'. defaults to 'short'
#' @param \dots Further arguments passed to internal function \code{call_limer()}.
#' @export
#' @examples \dontrun{
#' get_responses(475835)
#' }
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_activate_survey}

get_responses <- function(iSurveyID, sDocumentType = "csv", sLanguageCode = NULL,
                          sCompletionStatus = "complete", sHeadingType = "code",
                          sResponseType = "long", ...) {
  # Put all the function's arguments in a list to then be passed to call_limer()
  params <- as.list(environment())
  dots <- list(...)
  if (length(dots) > 0) params <- append(params, dots)
  # print(params) # uncomment to debug the params

  results <- call_limer(method = "export_responses", params = params)

  if (grepl("No Data, ", unlist(results))) {
    warning(unlist(results), call. = F)
    return(data.frame())
  }

  return(base64_to_df(unlist(results)))
}
