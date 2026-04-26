copy_survey_to <- function(iSurveyID, DestSurveyID = NULL, keep_title = TRUE, exclude_qids = NULL,
                           overwrite = TRUE) {

  base_url <- sub("/index.php/admin/remotecontrol", "", getOption("lime_api"))

  # Step 1: All API calls FIRST before creating UI session
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

  # Step 2: NOW create UI session and login
  s <- httr::handle(base_url)
  login_page <- httr::GET(handle = s, url = paste0(base_url, "/index.php/admin/authentication/sa/login"))
  page_text <- httr::content(login_page, as = "text", encoding = "utf-8")
  csrf_token <- regmatches(page_text, regexpr('(?<="csrfToken":")[^"]+', page_text, perl = TRUE))

  httr::POST(
    handle = s,
    url = paste0(base_url, "/index.php/admin/authentication/sa/login"),
    body = list(
      YII_CSRF_TOKEN = csrf_token,
      authMethod     = "Authdb",
      user           = getOption("lime_username"),
      password       = getOption("lime_password"),
      action         = "login",
      width          = "1920",
      login_submit   = "login",
      loginlang      = "default"
    ),
    encode = "form"
  )
  message("Logged in via UI session")

  # Step 3: Get CSRF from copy page
  copy_page <- httr::GET(handle = s, url = paste0(base_url, "/index.php/surveyAdministration/newSurvey?tab=copy"))
  page_text2 <- httr::content(copy_page, as = "text", encoding = "utf-8")
  csrf_token2 <- regmatches(page_text2, regexpr('(?<="csrfToken":")[^"]+', page_text2, perl = TRUE))

  # Step 4: Build copy title
  copy_title <- if (!is.null(original_title)) {
    original_title
  } else if (!is.null(DestSurveyID)) {
    as.character(DestSurveyID)
  } else {
    as.character(iSurveyID)
  }

  # Step 5: POST copy
  message("Copying survey ", iSurveyID, " to ", ifelse(is.null(DestSurveyID), "new survey", DestSurveyID), "...")
  body <- list(
    YII_CSRF_TOKEN                 = csrf_token2,
    copysurveylist                 = as.character(iSurveyID),
    copysurveyname                 = copy_title,
    sid                            = "0",
    copysurveytranslinksfields     = "1",
    copysurveyexcludequotas        = "0",
    copysurveyexcludepermissions   = "0",
    copysurveyexcludeanswers       = "0",
    copysurveyresetconditions      = "0",
    copysurveyresetstartenddate    = "0",
    copysurveyresetresponsestartid = "0"
  )
  if (!is.null(DestSurveyID)) {
    body$copysurveyid <- as.character(DestSurveyID)
  }

  httr::POST(
    handle = s,
    url = paste0(base_url, "/index.php/surveyAdministration/copy"),
    body = body,
    encode = "form"
  )

  # Step 6: Verify
  existing_after <- call_limer("list_surveys", params = list(NULL))
  ids_after <- as.character(existing_after$sid)

  if (!is.null(DestSurveyID)) {
    new_id <- as.character(DestSurveyID)
    new_row <- existing_after[ids_after == new_id, ]
    if (nrow(new_row) == 0) {
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
    new_row <- existing_after[ids_after == new_id, ]
  }

  message("✓ Survey ", new_id, " ready with title: '", new_row$surveyls_title, "'")

  # Step 7: Delete excluded questions by title
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
        result <- tryCatch(
          call_limer("delete_question", params = list(as.integer(qid))),
          error = function(e) {
            warning("Could not delete question ", qid, ": ", conditionMessage(e), call. = FALSE)
            NULL
          }
        )
        message("Deleted qid ", qid, " - result: ", as.character(result))
        Sys.sleep(0.5)
      }
      message("✓ Questions deleted")
    }
  }

  invisible(new_id)
}