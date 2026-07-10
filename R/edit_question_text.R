#' edit_question_text
#'
#' Updates the question text of an existing question in a survey. Accepts
#' either a numeric question ID or a question code (e.g. "G01Q03")
#'
#' @param survey_id integer, ID of the survey containing the question
#' @param question_id integer or string, either the numeric qid, or the
#' question's short code/title (e.g. "G01Q03")
#' @param new_text string, the new question text to set
#' @param language string, language code of the question text to update
#' (default "de")
#'
#' @return invisible result of the underlying set_question_properties call
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_set_question_properties
#' @examples
#' \dontrun{
#' edit_question_text(survey_id = 475835,
#'                    question_id = "G01Q03",
#'                    new_text = "New question text")
#' }
#' @export
edit_question_text <- function(survey_id,
                               question_id,
                               new_text,
                               language = "de") {
  survey_id <- as.integer(survey_id)
  if (is.na(survey_id))
    stop("Invalid survey_id \u2014 must be a numeric survey ID.", call. = FALSE)
  if (!nzchar(new_text))
    stop("new_text must be a non-empty string.", call. = FALSE)

  # question_id may be a qid (numeric) or a title/code like "G01Q03"
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

  # set_question_properties expects (qid, properties list, language)
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
