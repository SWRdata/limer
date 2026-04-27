copy_survey_to <- function(iSurveyID, DestSurveyID = NULL, keep_title = TRUE, exclude_qids = NULL) {

  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))

  # API calls first
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
      message("Deleting existing survey ", DestSurveyID, "...")
      call_limer("delete_survey", params = list(as.integer(DestSurveyID)))
    }
  }

  # UI session
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

  # Get CSRF from copy page
  p2 <- httr::content(httr::GET(handle = s, url = paste0(base_url, "/index.php/surveyAdministration/newSurvey?tab=copy")), as = "text", encoding = "utf-8")
  csrf2 <- regmatches(p2, regexpr('(?<="csrfToken":")[^"]+', p2, perl = TRUE))

  # POST copy
  message("Copying survey ", iSurveyID, " to ", ifelse(is.null(DestSurveyID), "new survey", DestSurveyID), "...")
  body <- list(
    YII_CSRF_TOKEN = csrf2, copysurveylist = as.character(iSurveyID),
    copysurveyname = if (!is.null(original_title)) original_title else if (!is.null(DestSurveyID)) as.character(DestSurveyID) else as.character(iSurveyID),
    sid = "0", copysurveytranslinksfields = "1", copysurveyexcludequotas = "0",
    copysurveyexcludepermissions = "0", copysurveyexcludeanswers = "0",
    copysurveyresetconditions = "0", copysurveyresetstartenddate = "0",
    copysurveyresetresponsestartid = "0"
  )
  if (!is.null(DestSurveyID)) body$copysurveyid <- as.character(DestSurveyID)
  httr::POST(handle = s, url = paste0(base_url, "/index.php/surveyAdministration/copy"), body = body, encode = "form")

  # Verify
  existing_after <- call_limer("list_surveys", params = list(NULL))
  ids_after <- as.character(existing_after$sid)

  if (!is.null(DestSurveyID)) {
    new_id <- as.character(DestSurveyID)
    if (!new_id %in% ids_after) {
      warning("Survey ", new_id, " not found after copy — copy may have failed.", call. = FALSE)
      return(invisible(NULL))
    }
  } else {
    new_ids <- setdiff(ids_after, ids_before)
    if (length(new_ids) == 0) {
      warning("Could not detect newly created survey — check manually.", call. = FALSE)
      return(invisible(NULL))
    }
    new_id <- new_ids[1]
  }
  message("Survey ", new_id, " ready with title: '", existing_after[ids_after == new_id, "surveyls_title"], "'")

  # Delete excluded questions
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
        tryCatch(
          call_limer("delete_question", params = list(as.integer(qid))),
          error = function(e) warning("Could not delete question ", qid, ": ", conditionMessage(e), call. = FALSE)
        )
        Sys.sleep(0.5)
      }
      message("Questions deleted")
    }
  }

  # Delete empty question groups
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