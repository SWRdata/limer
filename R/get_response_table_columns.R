#' get_response_table_columns
#'
#' Returns the column names of a survey's response table, as they would
#' appear in an exported response CSV. Uses the standard export_responses
#' RPC method rather than get_responses, since get_responses requires an
#' unofficial server-side patch that most installations won't have.
#'
#' @param iSurveyID integer, Id of the survey
#' @param verbose boolean, Giving out logging info
#' @param sHeadingType character, heading style for the returned column
#' names: "code" returns short question codes (e.g. "G01Q03"), "full"
#' returns the complete question text, "abbreviated" returns a
#' truncated/abbreviated version of the question text. Default = "code"
#' @param sLanguageCode character, language code to export in. Must match
#' one of the survey's configured languages. Default = "de"
#'
#' @return character vector of column names
#' @examples
#' \dontrun{
#' get_response_table_columns(475835,
#'                                      verbose = TRUE,
#'                                      sHeadingType = "full")
#' }
#' @export
get_response_table_columns <- function(iSurveyID, verbose = FALSE,
                                       sHeadingType = "code",
                                       sLanguageCode = "de") {
  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()
  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!",
         call. = F)

  # export_responses is a documented, unpatched RPC method - request just
  # one response (or none) since we only need the header row, not the
  # actual data
  raw <- call_limer(
    "export_responses",
    params = list(
      "iSurveyID" = iSurveyID,
      "sDocumentType" = "csv",
      "sLanguageCode" = sLanguageCode,
      "sCompletionStatus" = "all",
      "sHeadingType" = sHeadingType,
      "sResponseType" = "short"
    )
  )

  # export_responses returns an error object (list with a $status
  # message) instead of base64 data when something's wrong - e.g. an
  # invalid language code, an unactivated survey, or no matching
  # responses. Catch this before it hits base64_to_df() with a clear,
  # specific error rather than a cryptic base64-decoding failure.
  if (is.list(raw) && !is.null(raw$status))
    stop(
      glue::glue("Could not export responses for survey \u00b4{iSurveyID}\u00b4: {raw$status}"),
      call. = FALSE
    )

  df <- base64_to_df(raw)
  cols <- colnames(df)

  if (verbose)
    message(length(cols), " columns found for survey ", iSurveyID)

  return(cols)
}
