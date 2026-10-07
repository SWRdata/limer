#' Token fields needed for invite/reminder eligibility diagnosis.
#' @noRd
mail_eligibility_attribute_fields <- function() {
  c(
    "emailstatus", "sent", "completed",
    "remindersent", "remindercount", "usesleft"
  )
}

#' Fetch mail-relevant participant rows for the given tids (one API call).
#'
#' Uses [get_participants()] with an explicit attribute field list, because
#' LimeSurvey's list_participants only returns tid/token/name/email by default.
#'
#' @return list of one named list per requested tid (same order as `tid`)
#' @noRd
fetch_participant_mail_state <- function(iSurveyID, tid) {
  tid <- as.integer(tid)
  i_limit <- max(c(tid, 10000L), na.rm = TRUE)

  participants <- tryCatch(
    get_participants(
      iSurveyID = iSurveyID,
      bUnused = FALSE,
      tid = TRUE,
      aAttributes = mail_eligibility_attribute_fields(),
      use_attribute_labels = FALSE,
      iLimit = i_limit
    ),
    error = function(e) e
  )

  if (inherits(participants, "error")) {
    return(lapply(tid, function(one_tid) {
      list(
        tid = one_tid,
        found = FALSE,
        lookup_failed = TRUE,
        lookup_error = conditionMessage(participants)
      )
    }))
  }

  if (!is.data.frame(participants) || nrow(participants) == 0L) {
    return(lapply(tid, function(one_tid) {
      list(tid = one_tid, found = FALSE, lookup_failed = FALSE)
    }))
  }

  if (!"tid" %in% colnames(participants)) {
    return(lapply(tid, function(one_tid) {
      list(
        tid = one_tid,
        found = FALSE,
        lookup_failed = TRUE,
        lookup_error = "get_participants() did not return a tid column"
      )
    }))
  }

  participants$tid <- as.integer(participants$tid)

  lapply(tid, function(one_tid) {
    row <- participants[participants$tid == one_tid, , drop = FALSE]
    if (nrow(row) == 0L) {
      return(list(tid = one_tid, found = FALSE, lookup_failed = FALSE))
    }
    row <- row[1, , drop = FALSE]
    token_val <- mail_field_chr(row, "token")
    email_val <- mail_field_chr(row, "email")
    sent_val <- mail_field_chr(row, "sent")
    emailstatus_val <- mail_field_chr(row, "emailstatus")
    completed_val <- mail_field_chr(row, "completed")
    remindersent_val <- mail_field_chr(row, "remindersent")
    remindercount_val <- mail_field_chr(row, "remindercount")

    list(
      tid = one_tid,
      found = TRUE,
      lookup_failed = FALSE,
      token = token_val,
      email = email_val,
      sent = sent_val,
      emailstatus = emailstatus_val,
      completed = completed_val,
      remindersent = remindersent_val,
      remindercount = remindercount_val,
      has_token = !is.na(token_val) && nzchar(token_val),
      has_email = !is.na(email_val) && nzchar(email_val),
      would_match_bEmail_TRUE = identical(sent_val, "N") ||
        identical(sent_val, ""),
      would_match_bEmail_FALSE = !identical(sent_val, "N") &&
        !identical(sent_val, "") &&
        !is.na(sent_val) && nzchar(sent_val),
      emailstatus_ok = identical(emailstatus_val, "OK"),
      completed_open = identical(completed_val, "N") ||
        identical(completed_val, ""),
      invitation_sent = !identical(sent_val, "N") &&
        !identical(sent_val, "") &&
        !is.na(sent_val) && nzchar(sent_val)
    )
  })
}

#' Read a character field from a one-row data.frame, or NA if missing.
#' @noRd
mail_field_chr <- function(row, name) {
  if (!name %in% colnames(row)) {
    return(NA_character_)
  }
  val <- row[[name]][[1]]
  if (is.null(val) || length(val) == 0L || is.na(val)) {
    return(NA_character_)
  }
  as.character(val)
}

#' Max distinct error lines in mail eligibility messages.
#' @noRd
mail_eligibility_max_lines <- function() {
  10L
}

#' Format invite ineligibility: one line per distinct reason (max 10).
#' @noRd
format_invite_ineligibility <- function(state, bEmail) {
  reasons <- vapply(state, function(p) {
    invite_ineligibility_reason(p, bEmail)
  }, character(1))
  tids <- vapply(state, function(p) as.integer(p$tid), integer(1))
  collapse_mail_eligibility_lines(reasons, tids)
}

#' Format reminder ineligibility: one line per distinct reason (max 10).
#' @noRd
format_reminder_ineligibility <- function(state,
                                          iMinDaysBetween = NULL,
                                          iMaxReminders = NULL) {
  reasons <- vapply(state, function(p) {
    reminder_ineligibility_reason(p, iMinDaysBetween, iMaxReminders)
  }, character(1))
  tids <- vapply(state, function(p) as.integer(p$tid), integer(1))
  collapse_mail_eligibility_lines(reasons, tids)
}

#' Group identical reasons; emit at most mail_eligibility_max_lines() lines.
#' @noRd
collapse_mail_eligibility_lines <- function(reasons, tids,
                                            max_lines = mail_eligibility_max_lines()) {
  if (length(reasons) == 0L) {
    return("")
  }
  # Preserve first-seen order of distinct reasons
  uniq <- unique(reasons)
  n_uniq <- length(uniq)
  shown <- uniq[seq_len(min(n_uniq, max_lines))]

  lines <- vapply(shown, function(reason) {
    hit <- tids[reasons == reason]
    tid_lab <- format_tid_group_label(hit)
    paste0("- ", tid_lab, ": ", reason)
  }, character(1))

  if (n_uniq > max_lines) {
    lines <- c(
      lines,
      paste0(
        "- ... and ", n_uniq - max_lines,
        " more distinct issue(s) (",
        length(tids) - sum(reasons %in% shown),
        " tid(s))"
      )
    )
  }
  paste(lines, collapse = "\n")
}

#' Compact tid label: "tid 1", "tid 1, 2, 5", or "tid 1, 2, ... (1000 tids)".
#' @noRd
format_tid_group_label <- function(tids, max_show = 3L) {
  tids <- sort(unique(as.integer(tids)))
  n <- length(tids)
  if (n == 1L) {
    return(paste0("tid ", tids[[1]]))
  }
  if (n <= max_show) {
    return(paste0("tid ", paste(tids, collapse = ", ")))
  }
  head <- paste(tids[seq_len(max_show)], collapse = ", ")
  paste0("tid ", head, ", \u2026 (", n, " tids)")
}

#' Reason text for one invite participant (no tid prefix).
#' @noRd
invite_ineligibility_reason <- function(p, bEmail) {
  if (isTRUE(p$lookup_failed)) {
    err <- if (!is.null(p$lookup_error)) p$lookup_error else "unknown error"
    return(paste0("could not load participant (", err, ")"))
  }
  if (!isTRUE(p$found)) {
    return("not found in participant table")
  }

  issues <- character(0)
  if (!isTRUE(p$has_token)) {
    issues <- c(issues, paste0(
      "access token empty (token='') \u2014 use ",
      "add_participants(..., bCreateToken = TRUE) or add_participant_codes()"
    ))
  }
  if (!isTRUE(p$has_email)) {
    issues <- c(issues, "missing email")
  }
  if (!isTRUE(p$emailstatus_ok)) {
    issues <- c(issues, paste0(
      "emailstatus=", display_mail_field(p$emailstatus), " (required: 'OK')"
    ))
  }
  if (!isTRUE(p$completed_open)) {
    issues <- c(issues, paste0(
      "already completed (completed=",
      display_mail_field(p$completed), ")"
    ))
  }
  if (isTRUE(bEmail)) {
    if (isTRUE(p$has_token) && !isTRUE(p$would_match_bEmail_TRUE)) {
      issues <- c(issues, paste0(
        "invitation already sent (sent is not N/empty); ",
        "bEmail = TRUE only sends pending invites \u2014 use bEmail = FALSE to resend"
      ))
    }
  } else if (isTRUE(p$has_token) && !isTRUE(p$would_match_bEmail_FALSE)) {
    issues <- c(issues, paste0(
      "bEmail = FALSE requires prior invite, but sent is still N/empty \u2014 ",
      "use bEmail = TRUE for the first invite"
    ))
  }

  if (length(issues) == 0L) {
    return(paste0(
      "no matching LimeSurvey invite filters ",
      "(check token / sent / emailstatus / completed)"
    ))
  }
  paste(issues, collapse = "; ")
}

#' Reason text for one reminder participant (no tid prefix).
#' @noRd
reminder_ineligibility_reason <- function(p,
                                          iMinDaysBetween = NULL,
                                          iMaxReminders = NULL) {
  if (isTRUE(p$lookup_failed)) {
    err <- if (!is.null(p$lookup_error)) p$lookup_error else "unknown error"
    return(paste0("could not load participant (", err, ")"))
  }
  if (!isTRUE(p$found)) {
    return("not found in participant table")
  }

  issues <- character(0)
  if (!isTRUE(p$has_token)) {
    issues <- c(issues, paste0(
      "access token empty (token='') \u2014 use ",
      "add_participants(..., bCreateToken = TRUE) or add_participant_codes()"
    ))
  }
  if (!isTRUE(p$has_email)) {
    issues <- c(issues, "missing email")
  }
  if (!isTRUE(p$emailstatus_ok)) {
    issues <- c(issues, paste0(
      "emailstatus=", display_mail_field(p$emailstatus), " (required: 'OK')"
    ))
  }
  if (!isTRUE(p$completed_open)) {
    issues <- c(issues, paste0(
      "already completed (completed=",
      display_mail_field(p$completed), ")"
    ))
  }
  if (!isTRUE(p$invitation_sent)) {
    issues <- c(issues, paste0(
      "invitation not yet sent (sent is N/empty); ",
      "send an invitation before reminding"
    ))
  }
  if (!is.null(iMaxReminders) && !is.na(iMaxReminders)) {
    rc <- suppressWarnings(as.integer(p$remindercount))
    if (!is.na(rc) && rc >= as.integer(iMaxReminders)) {
      issues <- c(issues, paste0(
        "remindercount=", rc,
        " >= iMaxReminders=", as.integer(iMaxReminders)
      ))
    }
  }
  if (!is.null(iMinDaysBetween) && !is.na(iMinDaysBetween) &&
      !is.na(p$remindersent) && nzchar(p$remindersent) &&
      !identical(p$remindersent, "N")) {
    issues <- c(issues, paste0(
      "recent reminder may still be within iMinDaysBetween=",
      as.integer(iMinDaysBetween)
    ))
  }

  if (length(issues) == 0L) {
    extra <- if (!is.null(iMinDaysBetween) || !is.null(iMaxReminders)) {
      " and iMinDaysBetween / iMaxReminders"
    } else {
      ""
    }
    return(paste0(
      "no matching LimeSurvey reminder filters ",
      "(check sent / remindersent / remindercount / emailstatus / completed",
      extra,
      ")"
    ))
  }
  paste(issues, collapse = "; ")
}

#' Quote a field value for error messages.
#' @noRd
display_mail_field <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return("''")
  }
  paste0("'", as.character(x), "'")
}
