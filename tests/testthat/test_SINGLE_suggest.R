context("SINGLE suggest")
library(googleSuggestQueriesR)
library(testthat)

# defensive -------------------------------------------------------------------
test_that("query param doesn't accept wrong values", {
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = NA))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = NULL))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = ""))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = NaN))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = 0))
})

test_that("lang param doesn't accept wrong values", {
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", lang = NA))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", lang = NULL))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", lang = ""))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", lang = NaN))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", lang = "eng"))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", lang = 0))
})

test_that("lang param doesn't accept wrong values", {
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", interval = NA))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", interval = NULL))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", interval = ""))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", interval = NaN))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", interval = "eng"))
  expect_error(googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", interval = -1))
})

# main ------------------------------------------------------------------------
test_that("SINGLE returns some results and waits after the call", {
  start_time <- Sys.time()
  x <- NULL
  x <- googleSuggestQueriesR:::suggest_single_keyword(query = "mtcars", interval = 1)
  end_time <- Sys.time()
  diff_time <- difftime(end_time, start_time, units = "secs")

  # expect_vector(x)
  expect_gte(length(x), 3)
  expect_gte(diff_time, 4)
})
