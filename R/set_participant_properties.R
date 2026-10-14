#' set_participant_properties
#'
#' Allow to set properties about a specific participant, only one
#' participant can be updated.
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param aTokenQueryProperties list, query properties to identify the participant
#'   (e.g. `tid = 1`)
#' @param aTokenData list, key-value-pair of attribute name and value, typical
#' values are `remindercount`, `remindersent`, `completed`, `usesleft` or `sent`
#' see example and \url{https://api.limesurvey.org/classes/Token.html} for more
#' details
#' @param verbose boolean, Giving out logging info
#' @return none
#' @references \url{https://api.limesurvey.org/classes/remotecontrol-handle.html#method_set_participant_properties}
#' @references \url{https://api.limesurvey.org/classes/Token.html}
#' @examples
#' \dontrun{
#' set_participant_properties(
#'   iSurveyID = 475835,
#'   aTokenQueryProperties = list(tid = 1),
#'   aTokenData = list(
#'     remindercount = 0,
#'     remindersent = 0,
#'     completed = "N",
#'     usesleft = 1,
#'     sent = "N"
#'   ),
#'   verbose = TRUE
#' )
#' }
#' @export

set_participant_properties <- function(iSurveyID, aTokenQueryProperties,
                                       aTokenData, verbose = FALSE) {
  max_id <- max(as.numeric(aTokenQueryProperties))

  aTokenIDs_in_survey <- get_participants(iSurveyID, iStart = 1, iLimit = max_id, tid = TRUE) %>%
    dplyr::pull(.data$tid)

  if (!all(aTokenQueryProperties %>% unlist() %in% aTokenIDs_in_survey)) {
    warning("some Tid in the aTokenData not found in participants table", call. = F)
  }

  n <- length(aTokenQueryProperties)

  for (i in 1:n) {
    params <- list(
      "iSurveyID" = iSurveyID,
      "aTokenQueryProperties" = aTokenQueryProperties[i],
      aTokenData = aTokenData
    )


    resp <- call_limer(method = "set_participant_properties", params = params)
  }
  if (verbose) {
    message(glue::glue("{i} entrie(s) edited"))
  }
}
