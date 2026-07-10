#' get_participants
#'
#' Retrieves the list of participants of a survey. Large requests are
#' automatically split into chunks to avoid PHP memory issues on the
#' LimeSurvey server.
#'
#' @param iSurveyID integer, ID of the Survey to retrieve participants from
#' @param bUnused boolean, if TRUE, only unused tokens are returned
#' @param iStart integer, start id of the token list
#' @param iLimit integer, number of participants to return
#' @param tid boolean, if TRUE, includes the tid column in the result
#' @param chunksize integer, size of chunks used to split large requests
#' and avoid php memory problems
#' @param ... ellipsis parameters passed on to call_limer
#'
#' @return dataframe of participant data
#' @export
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_list_participants
#' @examples
#' \dontrun{
#' get_participants(475835)
#' }
get_participants <- function(iSurveyID,
                             bUnused = TRUE,
                             iStart = 1,
                             iLimit = 100,
                             tid = FALSE,
                             chunksize = 5000, ...){
  # helper: detect the API's "no participants" error response, which
  # comes back as a plain list (e.g. list(status = "...")) rather than a
  # data frame, and would otherwise break downstream data[row, col]
  # indexing with a cryptic "wrong number of dimensions" error
  is_error_response <- function(x) {
    is.list(x) && !is.data.frame(x) && !is.null(x$status)
  }

  # for php memory problems split iLimit in chunks
  if (iLimit > chunksize) {
    n <- iLimit/chunksize
    iLimit <- chunksize
    for (i in 1:n) {
      # param order matches the documented API order:
      # iSurveyID, iStart, iLimit, bUnused, aAttributes
      params <- list("iSurveyID" = iSurveyID, "iStart" = iStart, "iLimit" = iLimit, "bUnused" = bUnused)
      df <- call_limer(method = "list_participants", params = params, ...)

      if (is_error_response(df))
        stop(df$status, call. = FALSE)

      dfs <- lapply(df, data.frame, stringsAsFactors = FALSE)
      aTokenIDs <- limer::get_participants(iSurveyID, iStart = 1, iLimit = max_id, tid = TRUE, bUnused = FALSE) %>%
        suppressWarnings()
      data <- dplyr::bind_rows(dfs)
      if (nrow(data) > 0 && data[1,1] == "No survey participants found.")
        stop("No survey participants found.", call. = F)
      # set count
      iStart <- iStart + chunksize + 1
      cat("\r",round((i/n)*100, digits = 2), "%")
      utils::flush.console()
    }
  } else{
    params <- list("iSurveyID" = iSurveyID, "iStart" = iStart, "iLimit" = iLimit, "bUnused" = bUnused)
    data <- call_limer(method = "list_participants", params = params, ...)

    if (is_error_response(data))
      stop(data$status, call. = FALSE)
  }
  if (!tid)
    data <- data %>% dplyr::select(-tid)
  colnames(data) <- gsub("participant_info.","",colnames(data))
  cat("\r")
  utils::flush.console()
  return(data)
}

#' list_participants
#'
#' @description
#' Deprecated alias for [get_participants()]. Use that function instead.
#'
#' @inheritParams get_participants
#' @export
list_participants <- function(...) {
  .Deprecated("get_participants")
  get_participants(...)
}

