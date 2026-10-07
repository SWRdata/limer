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
#' @param aAttributes logical or character/list. \code{FALSE} (default) returns
#'   only base fields. \code{TRUE} loads all \code{attribute_*} fields from the
#'   survey's \code{attributedescriptions} and requests those by name (the API
#'   does not accept a bare boolean). A character vector or list of names (e.g.
#'   \code{c("attribute_1", "attribute_2")}) returns those fields only.
#' @param use_attribute_labels boolean, if TRUE (default) and attributes were
#'   requested, rename \code{attribute_N} columns to their
#'   \code{attributedescriptions} labels (e.g. \code{ags}, \code{gemeinde}),
#'   matching the names used when uploading via \code{add_participants}.
#'   Set to FALSE to keep API column names.
#' @param chunksize integer, size of chunks used to split large requests
#' and avoid php memory problems
#' @param ... ellipsis parameters passed on to call_limer (e.g. httr options).
#'   Common typos \code{limit} and \code{start} are remapped to \code{iLimit}
#'   and \code{iStart} with a message.
#'
#' @return dataframe of participant data. When attributes are requested and
#'   \code{use_attribute_labels = TRUE}, custom fields use their display
#'   names from the survey (as set at upload time).
#' @importFrom rlang .data
#' @export
#' @references https://api.limesurvey.org/classes/remotecontrol-handle.html#method_list_participants
#' @examples
#' \dontrun{
#' get_participants(475835)
#' get_participants(475835, aAttributes = TRUE)
#' get_participants(475835, aAttributes = c("attribute_1", "attribute_2"))
#' get_participants(475835, aAttributes = TRUE, use_attribute_labels = FALSE)
#' }
get_participants <- function(iSurveyID,
                             bUnused = TRUE,
                             iStart = 1,
                             iLimit = 100,
                             tid = FALSE,
                             aAttributes = FALSE,
                             use_attribute_labels = TRUE,
                             chunksize = 5000, ...) {
  # helper: detect the API's "no participants" error response, which
  # comes back as a plain list (e.g. list(status = "...")) rather than a
  # data frame, and would otherwise break downstream data[row, col]
  # indexing with a cryptic "wrong number of dimensions" error
  is_error_response <- function(x) {
    is.list(x) && !is.data.frame(x) && !is.null(x$status)
  }

  dots <- list(...)
  if ("limit" %in% names(dots)) {
    message(
      "`limit` is forwarded to `iLimit`. ",
      "Did you mean parameter 'iLimit'?"
    )
    iLimit <- dots$limit
    dots$limit <- NULL
  }
  if ("start" %in% names(dots)) {
    message(
      "`start` is forwarded to `iStart`. ",
      "Did you mean parameter 'iStart'?"
    )
    iStart <- dots$start
    dots$start <- NULL
  }

  aAttributes <- resolve_list_participants_a_attributes(
    iSurveyID,
    aAttributes
  )
  attributes_requested <- !identical(aAttributes, FALSE)

  # for php memory problems split iLimit in chunks
  if (iLimit > chunksize) {
    n <- iLimit / chunksize
    iLimit <- chunksize
    all_chunks <- list()
    for (i in 1:n) {
      # param order matches the documented API order:
      # iSurveyID, iStart, iLimit, bUnused, aAttributes
      params <- list(
        "iSurveyID" = iSurveyID,
        "iStart" = iStart,
        "iLimit" = iLimit,
        "bUnused" = bUnused,
        "aAttributes" = aAttributes
      )
      df <- do.call(
        call_limer,
        c(list(method = "list_participants", params = params), dots)
      )

      if (is_error_response(df)) {
        stop(df$status, call. = FALSE)
      }

      chunk_data <- participants_response_to_dataframe(df)
      if (nrow(chunk_data) > 0 && chunk_data[1, 1] == "No survey participants found.") {
        stop("No survey participants found.", call. = FALSE)
      }
      all_chunks[[i]] <- chunk_data
      iStart <- iStart + chunksize + 1
      cat("\r", round((i / n) * 100, digits = 2), "%")
      utils::flush.console()
    }
    data <- dplyr::bind_rows(all_chunks)
  } else {
    params <- list(
      "iSurveyID" = iSurveyID,
      "iStart" = iStart,
      "iLimit" = iLimit,
      "bUnused" = bUnused,
      "aAttributes" = aAttributes
    )
    data <- do.call(
      call_limer,
      c(list(method = "list_participants", params = params), dots)
    )

    if (is_error_response(data)) {
      stop(data$status, call. = FALSE)
    }
  }
  if (!tid && "tid" %in% colnames(data)) {
    data <- data %>% dplyr::select(-.data$tid)
  }
  colnames(data) <- gsub("participant_info.", "", colnames(data))

  if (attributes_requested && isTRUE(use_attribute_labels)) {
    descriptions <- tryCatch(
      get_attribute_descriptions(iSurveyID),
      error = function(e) list()
    )
    data <- rename_attributes_to_descriptions(data, descriptions)
  }

  cat("\r")
  utils::flush.console()
  return(data)
}

#' Normalize list_participants API result to a data frame.
#'
#' call_limer() may return a data frame (typical) or a list of participant
#' records. The chunked path must handle both; lapply(..., data.frame) on a
#' data frame incorrectly iterates over columns.
#' @noRd
participants_response_to_dataframe <- function(df) {
  if (is.data.frame(df)) {
    return(as.data.frame(df, stringsAsFactors = FALSE))
  }
  dfs <- lapply(df, data.frame, stringsAsFactors = FALSE)
  dplyr::bind_rows(dfs)
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
