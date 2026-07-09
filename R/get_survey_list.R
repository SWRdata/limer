#' get_survey_list
#'
#' If user is admin he can get surveys of every user (parameter sUsername) or
#' all surveys (sUsername=null).
#'
#' @param sUsername string, username
#' @param names boolean, Shall be returned only the names of the surveys.
#' @param sid boolean, Shall be given back only the Ids of the surveys.
#'
#' @return dataframe, survey titles (names), or survey ids (sid)
#' @export
#' @references https://api.limesurvey.org/classes/remotecontrol_handle.html#method_list_surveys
get_survey_list <- function(sUsername = NULL, names = FALSE, sid = TRUE) {
  res <- call_limer(
    "list_surveys",
    params = list("sUsername" = sUsername)
  )

  if ("status" %in% colnames(res) && res$status == "No surveys found") {
    warning("no surveys found!", call. = FALSE)
    return(data.frame(
      sid = NA, surveyls_title = NA, startdate = NA,
      expires = NA, active = NA
    ))
  }

  if (names) {
    return(res$surveyls_title)
  }
  if (sid) {
    return(res$sid)
  }

  return(res)
}

