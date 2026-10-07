test_that("get_participants expands aAttributes=TRUE to attribute name list", {
  captured <- NULL
  testthat::local_mocked_bindings(
    get_attribute_descriptions = function(iSurveyID) {
      list(
        attribute_1 = list(description = "ags"),
        attribute_2 = list(description = "gemeinde")
      )
    },
    call_limer = function(method, params = list(), ...) {
      captured <<- list(method = method, params = params)
      data.frame(
        tid = 1L,
        token = "abc",
        firstname = "Max",
        lastname = "Muster",
        email = "m@example.com",
        attribute_1 = "08111",
        stringsAsFactors = FALSE
      )
    },
    .package = "limer"
  )

  result <- get_participants(
    123,
    iStart = 1,
    iLimit = 10,
    bUnused = FALSE,
    tid = TRUE,
    aAttributes = TRUE
  )

  expect_equal(captured$method, "list_participants")
  expect_type(captured$params$aAttributes, "list")
  expect_equal(
    unlist(captured$params$aAttributes),
    c("attribute_1", "attribute_2")
  )
  expect_false(isTRUE(captured$params$aAttributes))
  # default use_attribute_labels = TRUE renames attribute_1 -> ags
  expect_true("ags" %in% colnames(result))
  expect_false("attribute_1" %in% colnames(result))
  expect_equal(result$ags, "08111")
})

test_that("get_participants renames attributes via descriptions", {
  testthat::local_mocked_bindings(
    get_attribute_descriptions = function(iSurveyID) {
      list(
        attribute_1 = list(description = "ags"),
        attribute_2 = list(description = "gemeinde")
      )
    },
    call_limer = function(method, params = list(), ...) {
      data.frame(
        tid = 1L,
        email = "m@example.com",
        attribute_1 = "08111",
        attribute_2 = "Stuttgart",
        stringsAsFactors = FALSE
      )
    },
    .package = "limer"
  )

  result <- get_participants(
    123,
    iLimit = 5,
    tid = TRUE,
    aAttributes = TRUE,
    use_attribute_labels = TRUE
  )

  expect_true(all(c("ags", "gemeinde") %in% colnames(result)))
  expect_false(any(grepl("^attribute_", colnames(result))))
  expect_equal(result$ags, "08111")
  expect_equal(result$gemeinde, "Stuttgart")
})

test_that("get_participants keeps attribute_* when use_attribute_labels = FALSE", {
  testthat::local_mocked_bindings(
    get_attribute_descriptions = function(iSurveyID) {
      list(
        attribute_1 = list(description = "ags"),
        attribute_2 = list(description = "gemeinde")
      )
    },
    call_limer = function(method, params = list(), ...) {
      data.frame(
        tid = 1L,
        email = "m@example.com",
        attribute_1 = "08111",
        attribute_2 = "X",
        stringsAsFactors = FALSE
      )
    },
    .package = "limer"
  )

  result <- get_participants(
    123,
    iLimit = 5,
    tid = TRUE,
    aAttributes = TRUE,
    use_attribute_labels = FALSE
  )

  expect_true(all(c("attribute_1", "attribute_2") %in% colnames(result)))
  expect_false("ags" %in% colnames(result))
})

test_that("rename_attributes_to_descriptions handles duplicates and missing", {
  df <- data.frame(
    attribute_1 = "a",
    attribute_2 = "b",
    attribute_3 = "c",
    stringsAsFactors = FALSE
  )
  descriptions <- list(
    attribute_1 = list(description = "ags"),
    attribute_2 = list(description = "ags"),
    attribute_3 = list(description = "")
  )
  out <- limer:::rename_attributes_to_descriptions(df, descriptions)
  expect_equal(colnames(out)[1], "ags")
  expect_equal(colnames(out)[2], "ags_1")
  expect_equal(colnames(out)[3], "attribute_3")
})

test_that("get_participants passes explicit character aAttributes as list", {
  captured <- NULL
  testthat::local_mocked_bindings(
    get_attribute_descriptions = function(iSurveyID) {
      list(attribute_2 = list(description = "gemeinde"))
    },
    call_limer = function(method, params = list(), ...) {
      captured <<- params
      data.frame(
        tid = 1L,
        email = "m@example.com",
        attribute_2 = "X",
        stringsAsFactors = FALSE
      )
    },
    .package = "limer"
  )

  result <- get_participants(
    123,
    iLimit = 5,
    tid = TRUE,
    aAttributes = c("attribute_1", "attribute_2")
  )

  expect_type(captured$aAttributes, "list")
  expect_equal(unlist(captured$aAttributes), c("attribute_1", "attribute_2"))
  expect_true("gemeinde" %in% colnames(result))
})

test_that("get_participants defaults aAttributes to FALSE", {
  captured <- NULL
  testthat::local_mocked_bindings(
    call_limer = function(method, params = list(), ...) {
      captured <<- params
      data.frame(tid = 1L, email = "m@example.com", stringsAsFactors = FALSE)
    },
    .package = "limer"
  )

  get_participants(123, iLimit = 5, tid = TRUE)
  expect_false(captured$aAttributes)
})

test_that("get_participants remaps limit/start aliases to iLimit/iStart", {
  captured <- NULL
  testthat::local_mocked_bindings(
    call_limer = function(method, params = list(), ...) {
      captured <<- params
      data.frame(tid = 1L, email = "m@example.com", stringsAsFactors = FALSE)
    },
    .package = "limer"
  )

  expect_message(
    get_participants(123, limit = 50, start = 3, tid = TRUE),
    "iLimit"
  )
  expect_equal(captured$iLimit, 50)
  expect_equal(captured$iStart, 3)
})

test_that("get_participants chunk path accepts data.frame from call_limer", {
  testthat::local_mocked_bindings(
    call_limer = function(method, params = list(), ...) {
      data.frame(
        tid = c(1L, 2L),
        token = c("abc", "def"),
        email = c("a@b.de", "c@d.de"),
        stringsAsFactors = FALSE
      )
    },
    .package = "limer"
  )

  result <- get_participants(
    123,
    iLimit = 10000,
    tid = TRUE,
    bUnused = FALSE
  )

  expect_equal(nrow(result), 4L)
  expect_true(all(c("tid", "token", "email") %in% colnames(result)))
})

test_that("resolve_list_participants_a_attributes never returns bare TRUE", {
  testthat::local_mocked_bindings(
    get_attribute_descriptions = function(iSurveyID) {
      list(attribute_1 = list(description = "x"))
    },
    .package = "limer"
  )
  resolved <- limer:::resolve_list_participants_a_attributes(1L, TRUE)
  expect_type(resolved, "list")
  expect_equal(unlist(resolved), "attribute_1")
})
