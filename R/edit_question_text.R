edit_question_text <- function(survey_id,
                               question_id,
                               new_text,
                               language = "de") {
  survey_id <- as.integer(survey_id)
  if (is.na(survey_id))
    stop("Invalid survey_id — must be a numeric survey ID.", call. = FALSE)
  if (!nzchar(new_text))
    stop("new_text must be a non-empty string.", call. = FALSE)

  # Resolve question title to numeric qid if needed
  qid <- suppressWarnings(as.integer(question_id))
  if (is.na(qid)) {
    message("Looking up qid for question title '", question_id, "'...")
    questions <- call_limer("list_questions", params = list(survey_id))
    match_row <- questions[questions$title == question_id, ]
    if (nrow(match_row) == 0)
      stop("No question found with title '", question_id, "' in survey ", survey_id, call. = FALSE)
    qid <- as.integer(match_row$qid[1])
    message("Resolved to qid: ", qid)
  }

  res <- call_limer(
    method = "set_question_properties",
    params = list(
      qid,
      list(
        question = new_text,
        language = language
      ),
      language
    )
  )

  message("Question ", question_id, " in survey ", survey_id, " updated successfully.")
  invisible(res)
}