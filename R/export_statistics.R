
#' export_statistics
#'
#' Export survey statistics
#' @param iSurveyID numeric ID of the Survey
#' @param sLanguage string language of the survey to use (default from Survey),
#' default "de"
#' @param graph boolean, Create graph option. Default FALSE
#' @param docType string, Type of documents the exported statistics should be
#' (pdf|xls|html)
#' @param filename string, path/name of the generateted File
#' @param verbose boolean, Giving out logging info
#' @param groupIDs integer, Group ID of question
#' @return file
#' @examples
#' \dontrun{
#' export_statistics(475835, filename = "statistics_example.pdf")
#' }
#' @export
#'
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_export_statistics
#' @note the function application/helpers/common_helper.php must be modified to make the PDF export work.
#' https://bugs.limesurvey.org/view.php?id=18049
#' https://raw.githubusercontent.com/LimeSurvey/LimeSurvey/a34c39ecf400599f25806db2e239053bc29af4ac/application/helpers/common_helper.php

export_statistics <- function(iSurveyID,
                              sLanguage = "de",
                              graph = FALSE,
                              docType = "pdf",
                              filename = NULL,
                              verbose = TRUE,
                              groupIDs = NULL){

  possible_output <- c("pdf", "html", "xls", "xlsx")
  docType <- tolower(docType)

  if (!docType %in% possible_output)
    stop(glue::glue("'docType' must be one of the following values: {paste(possible_output, collapse = ', ')}"))

  if (docType == "xlsx")
    docType <- "xls"

  if (!is.null(groupIDs)) {
    groupIDs <- as.numeric(groupIDs) %>% suppressWarnings()
    if (any(is.na(groupIDs)))
      stop("one or more groupIDs are not valid and must be of type integer", call. = F)
  }

  x <- call_limer("export_statistics",
                  params = list("iSurveyID" = iSurveyID,
                                "docType" = docType,
                                "sLanguage" = sLanguage,
                                "graph" = graph,
                                "groupIDs" = groupIDs
                                ))
  if (is.null(x))
    stop("Could not get data for evaluation")

  if (is.null(filename))
    filename <- glue::glue("limesurvey_survey_{iSurveyID}.{docType}")
  outconn <- file(filename,"wb")
  base64enc::base64decode(what = x, output = outconn)
  close(outconn)

  if (verbose)
    message(filename, " saved!")
}
