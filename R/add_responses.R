#' add_responses
#'
#' Inserts one or more answers into an answer table of a survey
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param verbose boolean, Giving out logging info
#' @param data dataframe, The actual response(s). Column names should be the
#' short question codes (e.g. "G01Q03") as shown in survey exports; these
#' are automatically translated to LimeSurvey's internal field names
#' (format \\verb{{iSurveyID}X{gid}X{qid}}) before submission."
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' responses_df <- data.frame(G01Q03 = c("1", "5"),
#'                                     G01Q04 = c("AO01", "AO03"),
#'                                     stringsAsFactors = FALSE)
#' add_responses(iSurveyID = 475835, data = responses_df, verbose = TRUE)
#' }
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_add_response
#' @export
add_responses <- function(iSurveyID, data, verbose = FALSE) {
  if (!inherits(data, "data.frame"))
    stop("Data must be of type data.frame", call. = F)

  survey_is_active <-
    get_survey_list(sid = F) %>%
    dplyr::filter(.data$sid == iSurveyID) %>%
    dplyr::pull(.data$active) == "Y"

  if (!survey_is_active)
    stop(
      "The survey is not active at the moment, therefore no answers can be imported. Please use `activate_survey()` to activate the survey.",
      call. = FALSE
    )

  # delete the ID to avoid collisions due to duplications and automatic
  # increments
  if ("id" %in% colnames(data) %>% tolower()) {
    data$id <- NULL
    if (verbose)
      warning("Column id was deleted to avoid collisions", call. = F)
  }

  # LimeSurvey's add_response API requires the full internal field name
  # ("{iSurveyID}X{gid}X{qid}"), not the short question code (e.g.
  # "G01Q03") shown in exports. Build a lookup from question code -> full
  # field name so we can translate the data frame's column names before
  # sending each row. NOTE: subquestion titles (SQ001, SQ002, ...) are not
  # unique across the survey, so this lookup is only reliable for
  # top-level, non-array question codes like "G01Q03".
  question_info <- call_limer(
    "list_questions",
    params = list("iSurveyID" = iSurveyID)
  )
  code_lookup <- stats::setNames(
    paste(iSurveyID, question_info$gid, question_info$qid, sep = "X"),
    question_info$title
  )

  missing_codes <- setdiff(colnames(data), names(code_lookup))
  if (length(missing_codes) > 0)
    stop(
      glue::glue(
        "The following column(s) do not match any question code in survey ",
        "\u00b4{iSurveyID}\u00b4: {paste(missing_codes, collapse = ', ')}"
      ),
      call. = FALSE
    )

  colnames(data) <- unname(code_lookup[colnames(data)])

  convert_column_types <- function(x) {
    if (all(!is.na(x), x != "")) {
      if (x == "F")
        x <- "FEMALE" # Rename for circumvent type.convert
      x <- utils::type.convert(x, as.is = TRUE)
      if (x == "FEMALE")
        x <- "F" # set to original value
    }
    return(x)
  }

  res <-
    apply(
      data,
      MARGIN = 1,
      FUN = function(x) {
        # remove NA Values and blanks
        x <- x[!is.na(x)] %>% trimws()
        x <- lapply(x, FUN = function(el) convert_column_types(el))
        call_limer("add_response",
                   params = list("iSurveyID" = iSurveyID,
                                 "aResponseData" = x))
      }
    )

  if ((length(res) == nrow(data)) &
      verbose & all(!is.na(res %>% as.numeric()))) {
    message("Responses successfully imported.")
  }
}
