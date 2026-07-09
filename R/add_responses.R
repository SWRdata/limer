

#' add_responses
#'
#' Inserts one or more answers into an answer table of a survey
#'
#' @param iSurveyID integer, ID of the Survey to insert responses
#' @param verbose boolean, Giving out logging info
#' @param data dataframe, The actual response(s)
#' @importFrom rlang .data
#'
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_add_response
#' @export

add_responses <- function(iSurveyID, data, verbose = FALSE) {
  if (!inherits(data, "data.frame"))
    stop("Data must be of type data.frame", call. = F)

  survey_is_active <-
    get_survey_list(sid = F) %>% dplyr::filter(.data$sid == iSurveyID) %>% dplyr::pull(.data$active) == "Y"

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


  # TODO
  # Clarify what happens to "No response" in the coding. For the correct import,
  # there must obviously be no NA for numeric values, as this will give an error
  # A 0 is interpreted as "Not finished or not shown".
  # https://manual.limesurvey.org/Import_responses#Reserved_names
  # Maybe this has to be determined for each question type?

  # data <- data %>%
  #   dplyr::mutate_if(is.numeric, ~tidyr::replace_na(., 0))

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

        x <- lapply(x, FUN = function(el) convert_column_types(el) )

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
