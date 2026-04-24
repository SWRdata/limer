#' import_survey_structure
#'
#' Import survey file in a known format into Limesurvey and create an inactive
#' survey with it.
#'
#' @param sImportData string, path to file
#' @param sNewSurveyName string, The optional new name of the survey
#' @param DestSurveyID integer This is the new ID of the survey - if already
#' used a random one will be taken instead
#' @param verbose boolean, Giving out logging info
#'
#' @export
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


    if (is.null(DestSurveyID) & grepl("lss$", sImportData)) {
      DestSurveyID <- xml2::read_xml(sImportData) %>%
        xml2::xml_find_all(xpath = "/document/groups/rows/row/sid") %>%
        xml2::xml_text()
    }

    if (is.null(sNewSurveyName) & grepl("lss$", sImportData)) {
      sNewSurveyName <- xml2::read_xml(sImportData) %>%
        xml2::xml_find_all(xpath = "//surveyls_title") %>%
        xml2::xml_text()
    }

    # If the ID already exists delete the existing survey
    if (DestSurveyID %in% limer::get_survey_list()) {
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

    if ((msg != DestSurveyID) & verbose)
      warning(
        glue::glue(
          "The Id of the survey already exists. The survey gets the new ID {msg}"
        ),
        call. = F
      )

    # Limesurvey ids are numeric
    if (!is.na(as.numeric(msg))) {
      if ((msg == DestSurveyID) & verbose)
          message("Survey with id \u00b4",
                  msg,
                  "\u00b4 from ",
                  sImportData,
                  " successfully imported")
    } else {
      stop(msg)
    }
  }
