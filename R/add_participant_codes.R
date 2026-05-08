add_participant_codes <- function(iSurveyID) {
  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))
  
  s <- httr::handle(base_url)
  p <- httr::content(httr::GET(handle = s, url = paste0(base_url, "/index.php/admin/authentication/sa/login")), as = "text", encoding = "utf-8")
  
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
  
  resp <- httr::GET(
    handle = s,
    url = paste0(base_url, "/index.php/admin/tokens/sa/tokenify/surveyid/", iSurveyID, "/ok/Y")
  )
  
  message("Tokens generated for survey ", iSurveyID)
  invisible(resp)
}
