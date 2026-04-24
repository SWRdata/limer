copy_survey_to <- function(iSurveyID,
                           DestSurveyID        = NULL,
                           exclude_qids        = NULL,
                           overwrite           = FALSE,
                           keep_title          = FALSE) {

  if (!overwrite && iSurveyID == DestSurveyID)
    stop("To replace existing survey please use reset_survey() or set overwrite = TRUE.", call. = F)

  iSurveyID <- as.numeric(iSurveyID) %>% suppressWarnings()
  if (is.na(iSurveyID))
    stop("No valid iSurveyID passed. iSurveyID must be a six-digit number!", call. = F)

  # Capture title of destination survey before overwriting
  original_title <- NULL
  if (overwrite && !is.null(DestSurveyID)) {
    existing_surveys <- list_surveys()
    dest_row <- existing_surveys[existing_surveys$sid == DestSurveyID, ]

    if (nrow(dest_row) > 0) {
      if (keep_title) {
        original_title <- dest_row$surveyls_title
        message("Preserving title of survey ", DestSurveyID, ": '", original_title, "'")
      }
      message("Deleting existing survey ", DestSurveyID, " before copy...")
      call_limer("delete_survey", params = list("iSurveyID" = as.integer(DestSurveyID)))
    }
  }

  # Copy the survey
  res <- call_limer("copy_survey_to",
                    params = list("iSurveyID"        = iSurveyID,
                                  "iDesiredSurveyId" = DestSurveyID))

  new_survey_id <- res

  # Restore original title if requested
  if (keep_title && !is.null(original_title)) {
    call_limer("set_survey_properties",
               params = list("iSurveyID"       = as.integer(new_survey_id),
                             "aSurveySettings" = list("surveyls_title" = original_title)))
    message("Title restored to: '", original_title, "'")
  }

  # Delete excluded questions from the copy
  if (!is.null(exclude_qids)) {
    exclude_qids <- as.integer(exclude_qids)
    if (any(is.na(exclude_qids)))
      stop("exclude_qids must be a vector of valid integer question IDs.", call. = F)

    message("Deleting ", length(exclude_qids), " question(s) from copied survey ", new_survey_id, "...")

    for (qid in exclude_qids) {
      tryCatch(
        call_limer("delete_question", params = list("iQuestionID" = qid)),
        error = function(e) {
          warning("Could not delete question ", qid, ": ", conditionMessage(e), call. = F)
        }
      )
    }
  }

  invisible(new_survey_id)
}
