#' activate_tokens
#'
#' Initialise the survey participant table of a survey where new participant
#' tokens may be later added.
#'
#' @param iSurveyID integer, ID of the Survey where a survey participants table
#' will be created for
#' @param aAttributeFields list or integer, An array of integers describing
#' any additional attribute fields (e.g. \code{list(1, 2)}), or a single
#' positive integer \code{n} meaning attributes 1..n.
#'
#' @return status
#' @references \url{https://api.limesurvey.org/classes/remotecontrol-handle.html#method_activate_tokens}
#' @examples
#' \dontrun{
#' activate_tokens("475835", aAttributeFields = list(1, 2))
#' activate_tokens("475835", aAttributeFields = 3)
#' }
#' @export
activate_tokens <- function(iSurveyID, aAttributeFields = NULL) {
  if (!is.null(aAttributeFields)) {
    if (is.numeric(aAttributeFields) && length(aAttributeFields) == 1L &&
        !is.list(aAttributeFields)) {
      n <- as.integer(aAttributeFields)
      if (is.na(n) || n < 1L) {
        stop("aAttributeFields must be a positive integer or a list of integers",
             call. = FALSE)
      }
      aAttributeFields <- as.list(seq_len(n))
    } else if (is.numeric(aAttributeFields) && !is.list(aAttributeFields)) {
      aAttributeFields <- as.list(as.integer(aAttributeFields))
    } else if (is.list(aAttributeFields)) {
      aAttributeFields <- lapply(aAttributeFields, as.integer)
    }
  }

  params <- list("iSurveyID" = iSurveyID, "aAttributeFields" = aAttributeFields)

  resp <- tryCatch({
    call_limer(method = "activate_tokens", params = params)
  }, error = function(e) {
    stop(gsub("^.*: [0-9]+|The SQL.*", "", e[["message"]]) %>% trimws(),
         call. = FALSE)
  })
  return(resp %>% unlist())
}


#' @rdname activate_tokens
#' @export
create_participants_table <- activate_tokens
