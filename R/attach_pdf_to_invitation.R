#' attach_pdf_to_invitation
#'
#' Attaches an already-uploaded PDF file as an email attachment to one of a
#' survey's email templates by
#' logging into the LimeSurvey admin interface and submitting the email
#' templates form directly. This works around the RemoteControl API by
#' scraping and reusing the admin web session, since editing email template
#' attachments is not exposed via the standard methods.
#'
#' @param survey_id integer, ID of the survey whose email template should
#' get the attachment
#' @param pdf_filename string, filename of the PDF as it exists under the
#' survey's uploaded files directory (e.g. "anfrage.pdf")
#' @param language string, language code of the email template to modify
#' (must match one of the survey's configured languages, default "de")
#' @param email_type string, which email template to attach the PDF to.
#' One of "invitation", "reminder", "confirmation", "registration",
#' "admin_notification", "admin_detailed_notification" (default
#' "invitation")
#'
#' @return invisible survey_id, returned for convenience/chaining
#' @examples
#' \dontrun{
#' attach_pdf_to_invitation(475835, pdf_filename = "anfrage.pdf")
#' }
#' @export
attach_pdf_to_invitation <- function(survey_id, pdf_filename,
                                     language = "de",
                                     email_type = "invitation") {
  # lime_api option points at the endpoint (.../admin/remotecontrol),
  # but the admin web UI and file uploads live one level up, so strip that
  # suffix off
  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))

  # Build the public URL of the PDF as LimeSurvey will reference it once
  # attached, and verify beforehand that the file actually exists at that
  # path - avoids silently attaching a dead link
  pdf_url <- paste0(base_url, "/upload/surveys/", survey_id, "/files/", pdf_filename)

  resp <- httr::GET(pdf_url)
  if (httr::status_code(resp) != 200)
    stop("PDF not found at: ", pdf_url, call. = FALSE)

  # httr::handle() keeps a persistent connection + cookie jar across
  # requests, which is required here since the login session (cookie) set
  # by the POST below must be reused for every subsequent request
  s <- httr::handle(base_url)

  # GET the login page first purely to scrape the CSRF token embedded in
  # its HTML/JS - LimeSurvey's admin login will reject the POST without it
  p <- httr::content(
    httr::GET(handle = s, url = paste0(base_url, "/index.php/admin/authentication/sa/login")),
    as = "text", encoding = "utf-8"
  )

  # Log in via the admin web form (not the API) so that we get a
  # regular authenticated browser session/cookie tied to handle `s`
  httr::POST(
    handle = s,
    url = paste0(base_url, "/index.php/admin/authentication/sa/login"),
    body = list(
      # Extract the token from `"csrfToken":"<value>"` in the page source
      YII_CSRF_TOKEN = regmatches(p, regexpr('(?<="csrfToken":")[^"]+', p, perl = TRUE)),
      authMethod = "Authdb", user = getOption("lime_username"),
      password = getOption("lime_password"), action = "login",
      # width/loginlang/login_submit are hidden form fields LimeSurvey's
      # login form expects; values mimic what a real browser submits
      width = "1920", login_submit = "login", loginlang = "default"
    ),
    encode = "form"
  )

  # Load the email templates admin page for this survey. This page's HTML
  # contains: a fresh CSRF token needed for the update POST below, and
  # the current values of every email subject/body field, which we must
  # resubmit unchanged alongside our new attachment - otherwise the update
  # form would overwrite them with blanks
  ep <- httr::content(
    httr::GET(handle = s, url = paste0(base_url, "/index.php/admin/emailtemplates/sa/index/surveyid/", survey_id)),
    as = "text", encoding = "utf-8"
  )
  csrf <- regmatches(ep, regexpr('(?<="csrfToken":")[^"]+', ep, perl = TRUE))

  # Helper: scrape the current value of an <input ... name="X" value="...">
  # field from the templates page HTML (used for email subject lines)
  get_field <- function(name) {
    pattern <- paste0('(?<=name="', name, '" value=")[^"]+')
    m <- regmatches(ep, regexpr(pattern, ep, perl = TRUE))
    if (length(m) == 0) "" else m
  }

  # Helper: scrape the current inner text of an element by id (used for
  # email body content, which is rendered as element content rather than
  # an input value attribute)
  get_body <- function(id) {
    pattern <- paste0('(?<=id="', id, '">)[^<]+')
    m <- regmatches(ep, regexpr(pattern, ep, perl = TRUE))
    if (length(m) == 0) "" else m
  }

  # Base fields required by the update form endpoint, independent of which
  # template/attachment we're actually changing
  body <- list(
    YII_CSRF_TOKEN = csrf,
    action         = "tokens",
    language       = language,
    save           = "save"
  )

  # The update form is a single POST that resaves *all* email templates at
  # once, so every subject/body field must be included even though we only
  # intend to change the attachment - otherwise LimeSurvey would wipe the
  # untouched templates. Re-scrape each field's current value from `ep`
  # and pass it straight back unchanged.
  for (type in c("invitation", "reminder", "confirmation", "registration",
                 "admin_notification", "admin_detailed_notification")) {
    body[[paste0("email_", type, "_subj_", language)]] <- get_field(paste0("email_", type, "_subj_", language))
    body[[paste0("email_", type, "_", language)]]      <- get_body(paste0("email_", type, "_", language))
  }

  # Add the actual attachment. LimeSurvey expects attachments as a nested
  # array keyed by [language][email_type][index][field] - "[0]" is the
  # first (only) attachment slot we're setting, "url" is the file location,
  # and "relevance" ("1" = always shown) controls whether it's included
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
