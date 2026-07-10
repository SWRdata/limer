#' import_survey_structure
#'
#' Imports a survey structure file (.lss) into LimeSurvey, creating a new
#' inactive survey from it. If no destination survey ID is given, one is
#' extracted automatically from the .lss file itself; if a survey already
#' exists at that ID, it is deleted first and replaced by the import.
#'
#' @param sImportData string, path to the structure file to import
#' @param sNewSurveyName string or NULL, the name to give the imported
#' survey. If NULL, the title is read from the file's own
#' `<surveyls_title>` node.
#' @param DestSurveyID integer or NULL, the survey ID to import into. If
#' NULL and the file is a .lss, the ID is auto-extracted from the file's
#' own group rows.
#' @param verbose boolean, Giving out logging info
#'
#' @export
#' @examples
#' \dontrun{
#' export_survey_structure(475835,
#'                        filename = "backup_475835.lss",
#'                        verbose = TRUE)
#' import_survey_structure(
#'  sImportData = "backup_475835.lss",
#'  sNewSurveyName = "Restored from backup",
#'  DestSurveyID = 475836,
#'  verbose = TRUE)
#' }
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_import_survey
import_survey_structure <-
  function(sImportData,
           sNewSurveyName = NULL,
           DestSurveyID = NULL,
           verbose = FALSE) {
    if (!file.exists(sImportData))
      stop(glue::glue(
        "Could not find the structure file {sImportData} in this location"
      ),
      call. = F)

    if (is.null(sNewSurveyName)) {
      sNewSurveyName <- xml2::read_xml(sImportData) %>%
        xml2::xml_find_first(xpath = "//surveyls_title") %>%
        xml2::xml_text()
    }

    # TODO
    # sImportData fileextension as parameter
    file_data <- base64enc::base64encode(sImportData)

    # Auto-extract the original survey ID from the .lss file's group rows.
    # Every group row carries the same <sid> (they all belong to one
    # survey), so multiple matches are expected here - take the first
    # unique value rather than the raw (possibly multi-element) result,
    # which would otherwise break the later if() checks below.
    if (is.null(DestSurveyID) & grepl("lss$", sImportData)) {
      DestSurveyID <- xml2::read_xml(sImportData) %>%
        xml2::xml_find_all(xpath = "/document/groups/rows/row/sid") %>%
        xml2::xml_text() %>%
        unique() %>%
        .[1]
    }

    if (is.null(sNewSurveyName) & grepl("lss$", sImportData)) {
      # xml_find_first (not xml_find_all) - only one title should exist,
      # and using xml_find_all here previously risked the same
      # "condition has length > 1" failure DestSurveyID had
      sNewSurveyName <- xml2::read_xml(sImportData) %>%
        xml2::xml_find_first(xpath = "//surveyls_title") %>%
        xml2::xml_text()
    }

    # If the ID already exists delete the existing survey. Guarded with
    # !is.null() since DestSurveyID may legitimately still be NULL here
    # (e.g. non-.lss import types), and DestSurveyID %in% NULL would
    # otherwise error/produce an unusable logical(0) for if().
    if (!is.null(DestSurveyID) && DestSurveyID %in% limer::get_survey_list()) {
      if (verbose)
        warning("A survey with this ID already exists and has been deleted.",
                call. = F)
      delete_survey(DestSurveyID, verbose = verbose)
    }

    msg <- call_limer(
      "import_survey",
      params = list(
        "sImportData" = file_data,
        "sImportDataType" = "lss",
        "sNewSurveyName" = sNewSurveyName,
        "DestSurveyID" = DestSurveyID
      )
    )

    # Same NULL-guard as above: only compare against DestSurveyID when we
    # actually had one requested
    if (!is.null(DestSurveyID) && (msg != DestSurveyID) & verbose)
      warning(
        glue::glue(
          "The Id of the survey already exists. The survey gets the new ID {msg}"
        ),
        call. = F
      )

    # Limesurvey ids are numeric - a non-numeric msg means the API
    # returned an error string instead of a new survey ID
    if (!is.na(suppressWarnings(as.numeric(msg)))) {
      if (verbose)
        message("Survey with id \u00b4",
                msg,
                "\u00b4 from ",
                sImportData,
                " successfully imported")
    } else {
      stop(msg)
    }
  }
