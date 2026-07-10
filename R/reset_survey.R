#' reset_survey
#'
#' Resets or copies a survey to its original state by re-importing its
#' structure. If no destination ID is given, resets the survey in place
#' (the existing survey at `iSurveyID` is replaced); if a destination ID
#' is given, creates a fresh copy there instead, leaving the original
#' untouched.
#'
#' @param iSurveyID integer, ID of the survey to reset (or copy from)
#' @param sImportData string, path to a structure file (.lss) to import.
#' If not set, the survey's current structure is exported and re-imported
#' as-is.
#' @param sNewSurveyName string, name for the resulting survey. If not
#' set, the name from `sImportData` (or the survey's current name) is used.
#' @param DestSurveyID integer, ID to import into. If not set, defaults to
#' `iSurveyID`, resetting the survey in place.
#' @param verbose boolean, giving out logging info
#'
#' @export
reset_survey <- function(iSurveyID, sImportData = NULL, sNewSurveyName = NULL,
                         DestSurveyID = NULL, verbose = FALSE) {
  if (is.null(sImportData)) {
    export_survey_structure(iSurveyID, filename = "tmp.lss", verbose = verbose)
    sImportData <- "tmp.lss"
  }

  if (is.null(DestSurveyID)) {
    DestSurveyID <- iSurveyID
    if (verbose)
      message(glue::glue("Resetting survey {iSurveyID} in place"))
  } else if (verbose) {
    message(glue::glue("Copying the survey to the new ID {DestSurveyID}"))
  }

  # import_survey_structure() already deletes any existing survey at
  # DestSurveyID before importing, so no separate delete_survey() call
  # is needed here
  import_survey_structure(sImportData, sNewSurveyName = sNewSurveyName,
                          DestSurveyID = DestSurveyID, verbose = verbose)

  # cleanup
  if (sImportData == "tmp.lss")
    file.remove("tmp.lss")
}
