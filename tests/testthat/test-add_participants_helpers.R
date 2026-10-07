test_that("extra_attribute_columns excludes known token fields", {
  fields <- c("email", "firstname", "ags", "gemeinde", "token")
  expect_equal(
    limer:::extra_attribute_columns(fields),
    c("ags", "gemeinde")
  )
})

test_that("build_attribute_descriptions maps names to attribute_N", {
  desc <- limer:::build_attribute_descriptions(c("ags", "gemeinde"))
  expect_equal(names(desc), c("attribute_1", "attribute_2"))
  expect_equal(desc$attribute_1$description, "ags")
  expect_equal(desc$attribute_2$description, "gemeinde")
  expect_equal(desc$attribute_1$mandatory, "N")
})

test_that("map_columns_to_attributes renames in order", {
  df <- data.frame(
    email = "a@b.de",
    ags = "1",
    gemeinde = "X",
    stringsAsFactors = FALSE
  )
  out <- limer:::map_columns_to_attributes(df, c("ags", "gemeinde"))
  expect_equal(colnames(out), c("email", "attribute_1", "attribute_2"))
  expect_equal(out$attribute_1, "1")
  expect_equal(out$attribute_2, "X")
})

test_that("match_attributes_by_description maps by description", {
  descriptions <- list(
    attribute_1 = list(description = "ags"),
    attribute_2 = list(description = "gemeinde")
  )
  mapping <- limer:::match_attributes_by_description(
    c("gemeinde", "ags"),
    descriptions
  )
  expect_equal(unname(mapping[["ags"]]), "attribute_1")
  expect_equal(unname(mapping[["gemeinde"]]), "attribute_2")
})

test_that("match_attributes_by_description marks missing as NA", {
  mapping <- limer:::match_attributes_by_description(c("ags", "foo"), list())
  expect_true(is.na(mapping[["ags"]]))
  expect_true(is.na(mapping[["foo"]]))
})

test_that("add_participants stops on invalid email when force = FALSE", {
  df <- data.frame(
    email = c("ok@example.com", "bad@"),
    firstname = c("A", "B"),
    stringsAsFactors = FALSE
  )
  expect_error(
    add_participants(1, df, ask = FALSE, force = FALSE),
    "Ungültige E-Mail"
  )
})

test_that("add_participants warns on invalid email when force = TRUE", {
  df <- data.frame(
    email = c("ok@example.com", "bad@"),
    firstname = c("A", "B"),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    exists_participants_table = function(...) TRUE,
    get_attribute_descriptions = function(...) list(),
    call_limer = function(...) {
      # Successful API payload for both rows (precheck already warned)
      list(
        list(email = "ok@example.com", tid = "1"),
        list(email = "bad@", tid = "2")
      )
    },
    .package = "limer"
  )
  expect_warning(
    add_participants(1, df, ask = FALSE, force = TRUE),
    "force = TRUE"
  )
})
