attach_pdf_to_invitation <- function(survey_id, pdf_filename, language = "de", email_type = "invitation") {

  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))
  pdf_url <- paste0(base_url, "/upload/surveys/", survey_id, "/files/", pdf_filename)

  # Check file exists
  resp <- httr::GET(pdf_url)
  if (httr::status_code(resp) != 200)
    stop("PDF not found at: ", pdf_url, call. = FALSE)

  # Login
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

  # Get email templates page
  ep <- httr::content(
    httr::GET(handle = s, url = paste0(base_url, "/index.php/admin/emailtemplates/sa/index/surveyid/", survey_id)),
    as = "text", encoding = "utf-8"
  )
  csrf <- regmatches(ep, regexpr('(?<="csrfToken":")[^"]+', ep, perl = TRUE))

  # Extract current field values
  get_field <- function(name) {
    pattern <- paste0('(?<=name="', name, '" value=")[^"]+')
    m <- regmatches(ep, regexpr(pattern, ep, perl = TRUE))
    if (length(m) == 0) "" else m
  }
  get_body <- function(id) {
    pattern <- paste0('(?<=id="', id, '">)[^<]+')
    m <- regmatches(ep, regexpr(pattern, ep, perl = TRUE))
    if (length(m) == 0) "" else m
  }

  # Build form body with all required fields
  body <- list(
    YII_CSRF_TOKEN = csrf,
    action         = "tokens",
    language       = language,
    save           = "save"
  )

  # Add all email subject/body fields
  for (type in c("invitation", "reminder", "confirmation", "registration",
                 "admin_notification", "admin_detailed_notification")) {
    body[[paste0("email_", type, "_subj_", language)]] <- get_field(paste0("email_", type, "_subj_", language))
    body[[paste0("email_", type, "_", language)]]      <- get_body(paste0("email_", type, "_", language))
  }

  # Add attachment
  body[[paste0("attachments[", language, "][", email_type, "][0][url]")]]       <- pdf_url
  body[[paste0("attachments[", language, "][", email_type, "][0][relevance]")]] <- "1"

  httr::POST(
    handle = s,
    url = paste0(base_url, "/index.php/admin/emailtemplates/sa/update/surveyid/", survey_id),
    body = body,
    encode = "form"
  )

  message("PDF attached to ", email_type, " email for survey ", survey_id)
  invisible(survey_id)
}




