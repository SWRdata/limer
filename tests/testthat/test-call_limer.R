test_that("call_limer errors if params is not a list", {
  expect_error(
    call_limer("list_surveys", params = "not_a_list"),
    "params must be a list"
  )
})

test_that("call_limer returns the parsed result on success", {
  fake_response <- list(
    status_code = function() 200
  )

  local_mocked_bindings(
    get_session_key = function(...) "fake_session_key",
    .package = "limer"
  )

  local_mocked_bindings(
    POST = function(...) {
      structure(list(), class = "response")
    },
    content = function(...) {
      '{"id":" ","result":{"sid":475835,"active":"Y"},"error":null}'
    },
    .package = "httr"
  )

  result <- call_limer("get_survey_properties", params = list(iSurveyID = 475835))

  expect_type(result, "list")
  expect_equal(result$sid, 475835)
  expect_equal(result$active, "Y")
})

test_that("call_limer errors with API error message when present", {
  local_mocked_bindings(
    get_session_key = function(...) "fake_session_key",
    .package = "limer"
  )

  local_mocked_bindings(
    POST = function(...) structure(list(), class = "response"),
    content = function(...) {
      '{"id":" ","result":null,"error":"Invalid session key"}'
    },
    .package = "httr"
  )

  expect_error(
    call_limer("list_surveys", params = list()),
    "Invalid session key"
  )
})

test_that("call_limer falls back to generic message when error is also null", {
  local_mocked_bindings(
    get_session_key = function(...) "fake_session_key",
    .package = "limer"
  )

  local_mocked_bindings(
    POST = function(...) structure(list(), class = "response"),
    content = function(...) {
      '{"id":" ","result":null,"error":null}'
    },
    .package = "httr"
  )

  expect_error(
    call_limer("nonexistent_method", params = list()),
    "nonexistent_method is an unknown function to remotecontrol"
  )
})


