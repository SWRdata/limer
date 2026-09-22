#' copy_survey_to
#'
#' Creates a copy of an existing survey, optionally overwriting a specific
#' destination survey ID, preserving its title, and pruning specific
#' questions and any resulting empty question groups from the copy
#' afterwards. This works around the RemoteControl API by combining RPC
#' calls (for lookups, deletions, and cleanup) with scraping and reusing an
#' admin web session for the actual copy action, since survey duplication
#' is not exposed via the standard RPC methods.
#'
#' @param iSurveyID integer, ID of the survey to copy from
#' @param DestSurveyID integer or NULL, ID of an existing survey to
#' overwrite with the copy. If NULL (default), a new survey is created
#' with a fresh auto-assigned ID instead of overwriting anything.
#' @param keep_title boolean, if TRUE and `DestSurveyID` refers to an
#' existing survey, the destination survey's original title is reused for
#' the copy instead of a generated one. Ignored if `DestSurveyID` is NULL
#' or does not currently exist.
#' @param exclude_qids character vector or NULL, question codes
#' (e.g. "G01Q03") to delete from the copy after it is created. Matching is
#' done by question title, not question ID, since IDs are reassigned when
#' a survey is copied. Any question groups left empty as a result of these
#' deletions are also removed automatically.
#'
#' @return invisible character, the ID of the newly created/overwritten
#' survey
#' @examples
#' \dontrun{
#' new_id <- copy_survey_to(iSurveyID = 475835,
#'                                  exclude_qids = c("G01Q03", "G01Q04"))
#' }
#' @export

copy_survey_to <- function(iSurveyID, DestSurveyID = NULL, keep_title = TRUE, exclude_qids = NULL) {

  # lime_api option points at the RPC endpoint (.../admin/remotecontrol),
  # but the admin web UI (needed for the copy action itself) lives one
  # level up, so strip that suffix off
  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))

  # Snapshot the full survey list
  existing_before <- call_limer("list_surveys", params = list(NULL))
  ids_before <- as.character(existing_before$sid)

  original_title <- NULL
  if (!is.null(DestSurveyID)) {
    dest_row <- existing_before[as.character(existing_before$sid) == as.character(DestSurveyID), ]
    if (nrow(dest_row) > 0) {
      if (keep_title) {
        original_title <- dest_row$surveyls_title
        message("Preserving title: '", original_title, "'")
      }
      # If DestSurveyID already exists, it needs to be deleted
      message("Deleting existing survey ", DestSurveyID, "...")
      call_limer("delete_survey", params = list(as.integer(DestSurveyID)))
    }
  }

  # httr::handle() keeps a persistent connection + cookie jar across
  # requests, which is required here since the login session (cookie) set
  # by the POST below must be reused for the copy request that follows
  s <- httr::handle(base_url)
  p <- httr::content(httr::GET(handle = s, url = paste0(base_url, "/index.php/admin/authentication/sa/login")), as = "text", encoding = "utf-8")

  # Log in via the admin web form (not the API) so that we get a
  # regular authenticated browser session/cookie tied to handle `s` -
  # required because survey copying is only exposed through the admin UI
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

  # The copy form needs its own fresh CSRF token, scraped from the "new
  # survey / copy" tab page itself (separate from the login page's token)
  p2 <- httr::content(httr::GET(handle = s, url = paste0(base_url, "/index.php/surveyAdministration/newSurvey?tab=copy")), as = "text", encoding = "utf-8")
  csrf2 <- regmatches(p2, regexpr('(?<="csrfToken":")[^"]+', p2, perl = TRUE))

  # Build and submit the copy form
  message("Copying survey ", iSurveyID, " to ", ifelse(is.null(DestSurveyID), "new survey", DestSurveyID), "...")
  body <- list(
    YII_CSRF_TOKEN = csrf2, copysurveylist = as.character(iSurveyID),
    # Title priority: preserved original title (if kept) > destination ID
    # as a fallback name > source survey ID as a last resort
    copysurveyname = if (!is.null(original_title)) original_title else if (!is.null(DestSurveyID)) as.character(DestSurveyID) else as.character(iSurveyID),
    sid = "0", copysurveytranslinksfields = "1", copysurveyexcludequotas = "0",
    copysurveyexcludepermissions = "0", copysurveyexcludeanswers = "0",
    copysurveyresetconditions = "0", copysurveyresetstartenddate = "0",
    copysurveyresetresponsestartid = "0"
  )
  # copysurveyid tells LimeSurvey to assign the copy this specific ID
  # rather than auto-generating one
  if (!is.null(DestSurveyID)) body$copysurveyid <- as.character(DestSurveyID)
  httr::POST(handle = s, url = paste0(base_url, "/index.php/surveyAdministration/copy"), body = body, encode = "form")

  # Verify the copy actually happened by re-listing surveys and comparing
  # against the "before" snapshot
  existing_after <- call_limer("list_surveys", params = list(NULL))
  ids_after <- as.character(existing_after$sid)

  if (!is.null(DestSurveyID)) {
    # Overwrite case: just confirm the destination ID exists again
    new_id <- as.character(DestSurveyID)
    if (!new_id %in% ids_after) {
      warning("Survey ", new_id, " not found after copy - copy may have failed.", call. = FALSE)
      return(invisible(NULL))
    }
  } else {
    # New-survey case: the new ID is whatever appeared in "after" that
    # wasn't present in "before"
    new_ids <- setdiff(ids_after, ids_before)
    if (length(new_ids) == 0) {
      warning("Could not detect newly created survey - check manually.", call. = FALSE)
      return(invisible(NULL))
    }
    new_id <- new_ids[1]
  }
  message("Survey ", new_id, " ready with title: '", existing_after[ids_after == new_id, "surveyls_title"], "'")

  # Delete excluded questions, if requested
  if (!is.null(exclude_qids)) {
    message("Looking up questions to exclude: ", paste(exclude_qids, collapse = ", "))
    questions <- call_limer("list_questions", params = list(as.integer(new_id)))
    to_delete <- questions[questions$title %in% exclude_qids, ]
    to_delete <- to_delete[order(to_delete$question_order, decreasing = TRUE), ]
    if (nrow(to_delete) == 0) {
      warning("No questions found matching: ", paste(exclude_qids, collapse = ", "), call. = FALSE)
    } else {
      message("Deleting ", nrow(to_delete), " question(s): ", paste(to_delete$title, collapse = ", "))
      for (qid in to_delete$qid) {
        # Wrap each deletion individually so one failure doesn't abort
        # the whole loop - warn and continue with the remaining questions
        tryCatch(
          call_limer("delete_question", params = list(as.integer(qid))),
          error = function(e) warning("Could not delete question ", qid, ": ", conditionMessage(e), call. = FALSE)
        )
        # Brief pause between deletions to avoid overwhelming the API
        Sys.sleep(0.5)
      }
      message("Questions deleted")
    }
  }

  # Clean up any question groups left with no questions in them as a
  # result of the deletions above
  groups <- call_limer("list_groups", params = list(as.integer(new_id)))
  remaining_questions <- call_limer("list_questions", params = list(as.integer(new_id)))

  empty_groups <- groups[!groups$gid %in% remaining_questions$gid, ]

  if (nrow(empty_groups) > 0) {
    message("Deleting ", nrow(empty_groups), " empty group(s): ", paste(empty_groups$group_name, collapse = ", "))
    for (gid in empty_groups$gid) {
      tryCatch(
        call_limer("delete_group", params = list(as.integer(new_id), as.integer(gid))),
        error = function(e) warning("Could not delete group ", gid, ": ", conditionMessage(e), call. = FALSE)
      )
    }
    message("Empty groups deleted")
  }

  invisible(new_id)
}
