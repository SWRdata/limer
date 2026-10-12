test_that("drop_tech_response_cols removes code meta fields and keeps token", {
  df <- data.frame(
    id = 1:2,
    submitdate = c("2024-01-01", "2024-01-02"),
    lastpage = c(1, 2),
    startlanguage = c("de", "de"),
    seed = c(1, 2),
    startdate = c("2024-01-01", "2024-01-02"),
    datestamp = c("2024-01-01", "2024-01-02"),
    ipaddr = c("1.1.1.1", "2.2.2.2"),
    refurl = c("", ""),
    token = c("abc", "def"),
    Q01 = c("A", "B"),
    stringsAsFactors = FALSE
  )
  out <- drop_tech_response_cols(df)
  expect_equal(colnames(out), c("token", "Q01"))
  expect_equal(out$token, c("abc", "def"))
  expect_equal(out$Q01, c("A", "B"))
})

test_that("drop_tech_response_cols keeps Zugangscode with German full headings", {
  df <- data.frame(
    `Antwort ID` = 1:2,
    `Datum abgeschickt` = c("2024-01-01", "2024-01-02"),
    `Datum gestartet` = c("2024-01-01", "2024-01-02"),
    `Letzte Seite` = c(1, 2),
    Startsprache = c("de", "de"),
    Zugangscode = c("abc", "def"),
    Frage = c("ja", "nein"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out <- drop_tech_response_cols(df)
  expect_equal(colnames(out), c("Zugangscode", "Frage"))
  expect_equal(out$Zugangscode, c("abc", "def"))
})

test_that("drop_empty_cols removes NA and blank-only columns", {
  df <- data.frame(
    token = c("a", "b"),
    empty_na = c(NA_character_, NA_character_),
    empty_blank = c("", "  "),
    empty_null_str = c("NULL", "n/a"),
    kept = c("x", ""),
    stringsAsFactors = FALSE
  )
  out <- drop_empty_cols(df)
  expect_equal(colnames(out), c("token", "kept"))
})

test_that("drop_empty_cols treats LimeSurvey-style '' firstname/lastname as empty", {
  df <- data.frame(
    token = c("a", "b"),
    firstname = c("", ""),
    lastname = c("", ""),
    email = c("a@x.de", "b@x.de"),
    stringsAsFactors = FALSE
  )
  out <- drop_empty_cols(df)
  expect_equal(colnames(out), c("token", "email"))
  expect_false("firstname" %in% colnames(out))
  expect_false("lastname" %in% colnames(out))
})

test_that("merge_participant_data drops empty firstname/lastname for matched tokens", {
  responses <- data.frame(
    token = c("abc", "def"),
    Q01 = c("A", "B"),
    stringsAsFactors = FALSE
  )
  # Unused token "zzz" has a name; matched tokens do not — firstname must go.
  participants <- data.frame(
    token = c("abc", "def", "zzz"),
    firstname = c("", "", "Ada"),
    lastname = c(NA_character_, "  ", "Lovelace"),
    email = c("a@x.de", "b@x.de", "c@x.de"),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_participants = function(...) participants
  )

  out <- merge_participant_data(responses, iSurveyID = 123)
  expect_false("firstname" %in% colnames(out))
  expect_false("lastname" %in% colnames(out))
  expect_true("email" %in% colnames(out))
  expect_equal(out$email, c("a@x.de", "b@x.de"))
})

test_that("response_token_col finds token and Zugangscode", {
  expect_equal(
    response_token_col(data.frame(token = "a", Q = 1)),
    "token"
  )
  df_de <- data.frame(
    Zugangscode = "a",
    Q = 1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_equal(response_token_col(df_de), "Zugangscode")
  expect_true(is.na(response_token_col(data.frame(Q = 1))))
})

test_that("merge_participant_data joins on token and drops empty cols", {
  responses <- data.frame(
    token = c("abc", "def"),
    Q01 = c("A", "B"),
    stringsAsFactors = FALSE
  )
  participants <- data.frame(
    token = c("abc", "zzz"),
    firstname = c("Ada", "Bob"),
    empty_attr = c("", NA_character_),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_participants = function(...) participants
  )

  out <- merge_participant_data(responses, iSurveyID = 123)
  expect_equal(nrow(out), 2)
  expect_equal(out$firstname, c("Ada", NA_character_))
  expect_false("empty_attr" %in% colnames(out))
  expect_equal(out$Q01, c("A", "B"))
})

test_that("merge_participant_data joins on Zugangscode", {
  responses <- data.frame(
    Zugangscode = c("abc", "def"),
    Q01 = c("A", "B"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  participants <- data.frame(
    token = c("abc", "def"),
    ags = c("08111", "08115"),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_participants = function(...) participants
  )

  out <- merge_participant_data(responses, iSurveyID = 123)
  expect_equal(out$ags, c("08111", "08115"))
  expect_true("Zugangscode" %in% colnames(out))
})

test_that("merge_participant_data warns when token column is missing", {
  responses <- data.frame(Q01 = c("A", "B"), stringsAsFactors = FALSE)
  expect_warning(
    out <- merge_participant_data(responses, iSurveyID = 123),
    "no token/Zugangscode"
  )
  expect_identical(out, responses)
})
