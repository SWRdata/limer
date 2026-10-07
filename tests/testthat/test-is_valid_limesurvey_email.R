test_that("is_valid_limesurvey_email accepts empty and NA", {
  expect_true(is_valid_limesurvey_email(""))
  expect_true(is_valid_limesurvey_email(NA_character_))
  expect_equal(
    is_valid_limesurvey_email(c("", NA_character_, "a@b.de")),
    c(TRUE, TRUE, TRUE)
  )
})

test_that("is_valid_limesurvey_email accepts normal addresses", {
  expect_true(is_valid_limesurvey_email("user@example.com"))
  expect_true(is_valid_limesurvey_email("m@aol.de"))
  expect_true(is_valid_limesurvey_email("vorname.nachname@gemeinde.example.de"))
})

test_that("is_valid_limesurvey_email rejects invalid addresses", {
  expect_false(is_valid_limesurvey_email("foo@"))
  expect_false(is_valid_limesurvey_email("foo"))
  expect_false(is_valid_limesurvey_email("a@b"))
  expect_false(is_valid_limesurvey_email("a@b."))
  expect_false(is_valid_limesurvey_email("user@domain\n.com"))
})

test_that("is_valid_limesurvey_email checks each of multiple addresses", {
  expect_true(is_valid_limesurvey_email("a@b.de;c@d.de"))
  expect_true(is_valid_limesurvey_email("a@b.de,c@d.de"))
  expect_false(is_valid_limesurvey_email("a@b.de;foo@"))
})

test_that("format_invalid_email_rows includes identifiers", {
  df <- data.frame(
    email = c("ok@a.de", "bad@"),
    ags = c("08111", "08112"),
    gemeinde = c("A", "B"),
    stringsAsFactors = FALSE
  )
  msg <- format_invalid_email_rows(df, 2L)
  expect_match(msg, "Zeile 2")
  expect_match(msg, "bad@")
  expect_match(msg, "ags='08112'")
  expect_match(msg, "gemeinde='B'")
})
