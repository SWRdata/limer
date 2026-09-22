#' delete_responses
#'
#' Deletes one or more responses
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param verbose boolean, Giving out logging info
#' @param iResponseIDs integer, response Ids to be deleted. If not set delete
#' all.
#'
#' @return -
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_activate_survey}
#' @examples
#' \dontrun{
#' delete_responses(475835)
#' }
#' @export

delete_responses <- function(iSurveyID, iResponseIDs = NULL, verbose = FALSE ) {

  if (!iSurveyID %in% limer::get_survey_list()) {
    stop(glue::glue("Survey with ID {iSurveyID} does not exist in this installation!"), call. = F)
  }

  if (is.null(iResponseIDs)) {
    iResponseIDs <- get_responses(iSurveyID)
    # Use the id column as ID if there are answers, otherwise set ID to 1
    if (nrow(iResponseIDs) > 0){
      iResponseIDs <- iResponseIDs$id
    } else{
      iResponseIDs <- 1
    }
  }

  del_response <- function(x, iSurveyID){

    call_limer("delete_response",
               params = list("iSurveyID" = iSurveyID,
                             "iResponseIDs" = x))
  }

  res <- lapply(iResponseIDs, FUN = function(x) del_response(x,iSurveyID ) )

  if (any(res %>% unlist() == "Response Id not found") & verbose) {
    message("one or more Id was not found and could not be deleted")
  }

  if (all(res %>% unlist() == "deleted") & verbose) {
    message("Responses successfully deleted")
  }

}

