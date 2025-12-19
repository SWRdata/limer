#' get_answer_options
#'
#' Extracts answer options for a specific question in a LimeSurvey survey
#'
#' @param iQuestionID integer, ID of the question
#'                    (can be extracted with get_questions_properties)
#' @param sUsername character, Username in Limesurvey installation
#'
#' @return Named character vector with answer codes as names and answer text as values
#' @export
#'
get_answer_options <- function(iQuestionID, sUsername = NULL) {
  # Get question properties from LimeSurvey
  question_props <- limer::call_limer(
    "get_question_properties",
    params = list("iQuestionID" = iQuestionID)
  )

  # Check if the question exists
  if (is.null(question_props) || is.null(question_props$qid)) {
    stop(glue::glue("Question with ID {iQuestionID} does not exist!"), call. = FALSE)
  }

  # Check if answeroptions exist
  if (is.null(question_props$answeroptions) ||
      length(question_props$answeroptions) == 0) {
    if (verbose) {
      message(glue::glue("No answer options found for question ID {iQuestionID}"))
    }
    return(character(0))
  }

  # Extract answer text from each option
  answer_vector <- sapply(question_props$answeroptions, function(option) {
    option$answer
  })

  names(answer_vector) <- names(question_props$answeroptions)

  return(answer_vector)
}
