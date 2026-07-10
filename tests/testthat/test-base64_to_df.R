test_that("base64_to_df decodes a normal CSV correctly", {
  csv_text <- "col1;col2\r\n1;a\r\n2;b\r\n"
  encoded <- base64enc::base64encode(charToRaw(csv_text))

  result <- base64_to_df(encoded)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(colnames(result), c("col1", "col2"))
  expect_equal(result$col1, c(1, 2))
  expect_equal(result$col2, c("a", "b"))
})

test_that("base64_to_df returns empty data frame with warning for empty input", {
  encoded_empty <- base64enc::base64encode(charToRaw("\r\n"))

  expect_warning(
    result <- base64_to_df(encoded_empty),
    "empty data table"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})

test_that("base64_to_df preserves UTF-8 / German umlauts", {
  csv_text <- "Frage;Antwort\r\n1;Größe\r\n2;Straße\r\n"
  encoded <- base64enc::base64encode(charToRaw(csv_text))

  result <- base64_to_df(encoded)

  expect_equal(result$Antwort, c("Größe", "Straße"))
})

test_that("base64_to_df handles a single-column CSV", {
  csv_text <- "onlycol\r\nx\r\ny\r\n"
  encoded <- base64enc::base64encode(charToRaw(csv_text))

  result <- base64_to_df(encoded)

  expect_equal(ncol(result), 1)
  expect_equal(result$onlycol, c("x", "y"))
})