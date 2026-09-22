#' delete_participants
#'
#' delete participants from a survey by id
#'
#' @param iSurveyID integer, ID of the Survey to delete participants
#' @param aTokenIDs integer, Vector with the ID in the participant table
#' @param chunksize integer, size of chunks for handling php memory problems
#' @param max_id integer, up to which maximum id should the participants be
#' deleted? only necessary if aTokenIDs is NULL
#' @param ask boolean, if TRUE (default) asks for interactive confirmation
#' before deleting. Set to FALSE for non-interactive/scripted use.
#'
#' @references \url{https://api.limesurvey.org/classes/remotecontrol_handle.html#method_delete_participants}
#' @examples
#' \dontrun{
#' delete_participants(475835)
#' }
#' @export
delete_participants <-
  function(iSurveyID,
           aTokenIDs = NULL,
           max_id = 1000000,
           chunksize = 5000,
           ask = TRUE) {
    options(scipen = 999)
    if (ask) {
      cat(glue::glue("Delete all participants up to the maximum id number `{max_id}`. Proceed?[Y/N]"))
      answer <- readline() %>% tolower()
      if (tolower(answer) != "y")
        return("end without deleting participants")
    }
    if (is.null(aTokenIDs)) {
      # tid = TRUE is required here - get_participants() strips the tid
      # column by default, which previously broke the dplyr::pull(tid)
      # call below with "Column `tid` not found"
      aTokenIDs <- limer::get_participants(iSurveyID, iStart = 1, iLimit = max_id, tid = TRUE) %>%
        suppressWarnings()
      if (class(aTokenIDs)[1] == "list")
        return(warning(aTokenIDs$status, call. = F) %>% unlist())
      if (nrow(aTokenIDs) == 0)
        return(invisible(NULL))
      aTokenIDs <- aTokenIDs %>%
        dplyr::pull(.data$tid) %>% as.list()
    } else {
      aTokenIDs <- as.list(aTokenIDs)
    }
    names(aTokenIDs) <- rep("aTokenIDs", length(aTokenIDs))
    n <- ceiling(length(aTokenIDs)/chunksize)
    limit <- 0
    for (i in 1:n) {
      params <-
        list("iSurveyID" = iSurveyID, "aTokenIDs" = as.list(aTokenIDs[limit:(limit + chunksize)]))
      resp <-
        call_limer(method = "delete_participants", params = params)
      limit <- limit + chunksize + 1
    }
  }
