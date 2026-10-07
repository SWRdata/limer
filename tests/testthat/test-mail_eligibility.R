eligible_base <- function(tid = 1L) {
  list(
    tid = tid,
    found = TRUE,
    lookup_failed = FALSE,
    token = "abc123",
    email = "a@b.de",
    sent = "N",
    emailstatus = "OK",
    completed = "N",
    remindersent = "N",
    remindercount = "0",
    has_token = TRUE,
    has_email = TRUE,
    would_match_bEmail_TRUE = TRUE,
    would_match_bEmail_FALSE = FALSE,
    emailstatus_ok = TRUE,
    completed_open = TRUE,
    invitation_sent = FALSE
  )
}

test_that("format_invite_ineligibility reports empty token", {
  p <- eligible_base(1L)
  p$token <- ""
  p$has_token <- FALSE
  msg <- format_invite_ineligibility(list(p), bEmail = TRUE)
  expect_match(msg, "tid 1")
  expect_match(msg, "access token empty")
  expect_match(msg, "bCreateToken")
})

test_that("format_invite_ineligibility reports bEmail FALSE without prior invite", {
  p <- eligible_base(2L)
  msg <- format_invite_ineligibility(list(p), bEmail = FALSE)
  expect_match(msg, "tid 2")
  expect_match(msg, "bEmail = FALSE requires prior invite")
  expect_match(msg, "sent is still N/empty")
})

test_that("format_invite_ineligibility reports already sent with bEmail TRUE", {
  p <- eligible_base(3L)
  p$sent <- "2024-01-01 12:00"
  p$would_match_bEmail_TRUE <- FALSE
  p$would_match_bEmail_FALSE <- TRUE
  p$invitation_sent <- TRUE
  msg <- format_invite_ineligibility(list(p), bEmail = TRUE)
  expect_match(msg, "invitation already sent")
  expect_match(msg, "bEmail = FALSE to resend")
})

test_that("format_invite_ineligibility reports emailstatus and missing tid", {
  bad_status <- eligible_base(4L)
  bad_status$emailstatus <- "Invalid"
  bad_status$emailstatus_ok <- FALSE
  missing <- list(tid = 9L, found = FALSE, lookup_failed = FALSE)
  msg <- format_invite_ineligibility(list(bad_status, missing), bEmail = TRUE)
  expect_match(msg, "emailstatus='Invalid'")
  expect_match(msg, "tid 9: not found in participant table")
  expect_equal(length(strsplit(msg, "\n")[[1]]), 2L)
})

test_that("format_reminder_ineligibility reports invitation not sent", {
  p <- eligible_base(5L)
  msg <- format_reminder_ineligibility(list(p))
  expect_match(msg, "tid 5")
  expect_match(msg, "invitation not yet sent")
})

test_that("format_reminder_ineligibility reports max reminders", {
  p <- eligible_base(6L)
  p$sent <- "2024-01-01"
  p$invitation_sent <- TRUE
  p$would_match_bEmail_TRUE <- FALSE
  p$would_match_bEmail_FALSE <- TRUE
  p$remindercount <- "3"
  msg <- format_reminder_ineligibility(list(p), iMaxReminders = 3L)
  expect_match(msg, "remindercount=3")
  expect_match(msg, "iMaxReminders=3")
})

test_that("identical errors collapse to one line with tid count", {
  state <- lapply(1:20, function(i) {
    p <- eligible_base(i)
    p$token <- ""
    p$has_token <- FALSE
    p
  })
  msg <- format_invite_ineligibility(state, bEmail = TRUE)
  lines <- strsplit(msg, "\n")[[1]]
  expect_equal(length(lines), 1L)
  expect_match(msg, "20 tids")
  expect_match(msg, "access token empty")
})

test_that("distinct errors get separate lines up to max 10", {
  a <- eligible_base(1L)
  a$token <- ""
  a$has_token <- FALSE
  b <- eligible_base(2L)
  b$email <- ""
  b$has_email <- FALSE
  msg <- format_invite_ineligibility(list(a, b), bEmail = TRUE)
  expect_match(msg, "tid 1:.*access token empty")
  expect_match(msg, "tid 2:.*missing email")
  expect_equal(length(strsplit(msg, "\n")[[1]]), 2L)
})

test_that("more than 10 distinct reasons are capped", {
  state <- lapply(1:15, function(i) {
    p <- eligible_base(i)
    p$emailstatus <- paste0("bad_", i)
    p$emailstatus_ok <- FALSE
    p
  })
  msg <- format_invite_ineligibility(state, bEmail = TRUE)
  lines <- strsplit(msg, "\n")[[1]]
  # 10 detail lines + 1 "... and N more" summary
  expect_equal(length(lines), 11L)
  expect_match(msg, "more distinct issue")
})

test_that("format_tid_group_label formats compact tid lists", {
  expect_equal(format_tid_group_label(1L), "tid 1")
  expect_equal(format_tid_group_label(c(1L, 2L)), "tid 1, 2")
  expect_equal(
    format_tid_group_label(1:5),
    "tid 1, 2, 3, … (5 tids)"
  )
})
