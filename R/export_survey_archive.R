#' export_survey_archive
#'
#' Exports a survey archive (.lsa file) for a survey, including its
#' structure, responses, and settings. Uses the LimeSurvey admin web UI's
#' built-in export feature (Tools > Export > Umfragearchiv) rather than
#' the RemoteControl API, since the RPC method for this requires an
#' unofficial server-side patch that most installations won't have.
#'
#' @param iSurveyID integer, ID of the survey to export
#' @param filename string or NULL, path to save the .lsa file to. If NULL
#' (default), saves as \verb{limesurvey_survey_{iSurveyID}.lsa} in the current
#' working directory.
#' @param verbose boolean, Giving out logging info
#'
#' @return invisible path to the saved .lsa file
#' @export
#' @examples
#' \dontrun{
#' export_survey_archive(475835,
#'                                 filename = "archive_example.lsa",
#'                                 verbose = TRUE)
#' }

export_survey_archive <- function(iSurveyID, filename = NULL, verbose = FALSE) {
  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))

  s <- httr::handle(base_url)
  p <- httr::content(
    httr::GET(handle = s, url = paste0(base_url, "/index.php/admin/authentication/sa/login")),
    as = "text", encoding = "utf-8"
  )

  httr::POST(
    handle = s,
    url = paste0(base_url, "/index.php/admin/authentication/sa/login"),
    body = list(
      YII_CSRF_TOKEN = regmatches(p, regexpr('(?<="csrfToken":")[^"]+', p, perl = TRUE)),
      authMethod = "Authdb", user = getOption("lime_username"),
      password = getOption("lime_password"), action = "login",
      width = "1920", login_submit = "login", loginlang = "default"
    ),
    encode = "form"
  )

  if (is.null(filename))
    filename <- glue::glue("limesurvey_survey_{iSurveyID}.lsa")

  resp <- httr::GET(
    handle = s,
    url = paste0(base_url, "/index.php/admin/export/sa/survey/action/exportarchive/surveyid/", iSurveyID),
    httr::write_disk(filename, overwrite = TRUE)
  )

  if (httr::status_code(resp) != 200) {
    unlink(filename)
    stop("Failed to export survey ", iSurveyID, " (HTTP ", httr::status_code(resp), ")", call. = FALSE)
  }

  if (verbose)
    message(filename, " saved!")

  invisible(filename)
}
