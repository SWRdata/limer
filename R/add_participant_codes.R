#' add_participant_codes
#'
#' Generates participant tokens/codes for a survey by logging into the
#' LimeSurvey admin interface and triggering the tokenify action. This works
#' around the RemoteControl API by scraping and reusing the admin web
#' session, since token generation for existing participants is not exposed
#' via the standard RPC methods.
#'
#' @param iSurveyID integer, ID of the survey for which participant tokens
#' should be generated
#'
#' @return invisible httr response object from the tokenify request
#' @example add_participant_codes(475835)
#' @export

add_participant_codes <- function(iSurveyID) {
  # lime_api option points at the endpoint (.../admin/remotecontrol),
  # but the admin web UI lives one level up, so strip that suffix off
  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))

  # httr::handle() keeps a persistent connection + cookie jar across
  # requests, which is required here since the login session (cookie) set
  # by the POST below must be reused for the GET request
  s <- httr::handle(base_url)

  # GET the login page first purely to scrape the token embedded in
  # its HTML - LimeSurvey's admin login will reject the POST without it
  p <- httr::content(
    httr::GET(
      handle = s,
      url = paste0(base_url, "/index.php/admin/authentication/sa/login")
    ),
    as = "text",
    encoding = "utf-8"
  )

  # Log in via the admin web form so that we get a
  # regular authenticated browser session/cookie tied to handle `s`
  httr::POST(
    handle = s,
    url = paste0(base_url, "/index.php/admin/authentication/sa/login"),
    body = list(
      # Extract the token from `"csrfToken":"<value>"` in the page source
      YII_CSRF_TOKEN = regmatches(
        p, regexpr('(?<="csrfToken":")[^"]+', p, perl = TRUE)
      ),
      authMethod = "Authdb", user = getOption("lime_username"),
      password = getOption("lime_password"), action = "login",
      # width/loginlang/login_submit are hidden form fields LimeSurvey's
      # login form expects; values mimic what a real browser submits
      width = "1920", login_submit = "login", loginlang = "default"
    ),
    encode = "form"
  )

  # With the session cookie now set on `s`, hit the tokenify admin action
  # directly - "ok/Y" confirms the action without an extra confirmation
  # screen, which is what actually generates the participant tokens
  resp <- httr::GET(
    handle = s,
    url = paste0(
      base_url, "/index.php/admin/tokens/sa/tokenify/surveyid/",
      iSurveyID, "/ok/Y"
    )
  )

  message("Tokens generated for survey ", iSurveyID)
  invisible(resp)
}
