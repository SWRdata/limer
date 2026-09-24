#' export_survey_quexml
#'
#' Exports a survey as queXML PDF ZIP (PDF, queXML, banding XML, style XML,
#' readme) via the RemoteControl API. Requires the unofficial
#' \code{export_survey_quexml} method on the LimeSurvey server (see
#' \code{remote_control_patch.php} in the companion LimeSurvey ops repo).
#'
#' @param iSurveyID integer, ID of the survey to export
#' @param filename string or NULL, path to save the .zip file to. If NULL
#' (default), saves as \verb{limesurvey_survey_{iSurveyID}_quexml.zip} in
#' the current working directory. A ".zip" extension is appended
#' automatically if missing.
#' @param language string or NULL, survey language for the export. If NULL,
#' the survey base language is used on the server.
#' @param settings named list or NULL, optional queXML layout/style settings
#' (e.g. \code{queXMLPageFormat}, \code{queXMLStyle}). NULL uses server
#' defaults.
#' @param verbose boolean, Giving out logging info
#'
#' @return invisible path to the saved .zip file
#' @export
#' @examples
#' \dontrun{
#' export_survey_quexml(825549, filename = "medienanfrage_quexml.zip")
#' }
export_survey_quexml <- function(iSurveyID,
                                 filename = NULL,
                                 language = NULL,
                                 settings = NULL,
                                 verbose = FALSE) {
  # Only send optional args when set; omitted params stay PHP defaults.
  # (Sending R NULL as JSON {} previously caused HTTP 500 on the server.)
  params <- list(as.integer(iSurveyID))
  if (!is.null(language) || !is.null(settings)) {
    params <- c(params, list(language, settings))
  }

  x <- call_limer("export_survey_quexml", params = params)

  if (is.list(x) && !is.null(x$status)) {
    err <- if (!is.null(x$error)) x$error else x$status
    stop(
      glue::glue(
        "Could not export queXML for survey \u00b4{iSurveyID}\u00b4: {err}"
      ),
      call. = FALSE
    )
  }

  if (is.null(x) || !is.character(x) || !nzchar(x)) {
    stop(
      glue::glue(
        "Could not export queXML for survey \u00b4{iSurveyID}\u00b4: empty response"
      ),
      call. = FALSE
    )
  }

  if (is.null(filename)) {
    filename <- glue::glue("limesurvey_survey_{iSurveyID}_quexml.zip")
  } else if (!grepl("\\.zip$", filename, ignore.case = TRUE)) {
    filename <- glue::glue("{filename}.zip")
  }

  outconn <- file(filename, "wb")
  on.exit(close(outconn), add = TRUE)
  base64enc::base64decode(what = x, output = outconn)

  if (verbose) {
    message(filename, " saved!")
  }

  invisible(filename)
}
