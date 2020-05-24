context("MAIN suggest")
library(googleSuggestQueriesR)
library(testthat)

# defensive -------------------------------------------------------------------
test_that("queries param doesn't accept wrong values", {
  expect_error(suggest_keywords(queries = NA))
  expect_error(suggest_keywords(queries = NULL))
  expect_error(suggest_keywords(queries = ""))
  expect_error(suggest_keywords(queries = NaN))
  expect_error(suggest_keywords(queries = 0))
})

test_that("lang param doesn't accept wrong values", {
  expect_error(suggest_keywords(queries = "mtcars", lang = NA))
  expect_error(suggest_keywords(queries = "mtcars", lang = NULL))
  expect_error(suggest_keywords(queries = "mtcars", lang = ""))
  expect_error(suggest_keywords(queries = "mtcars", lang = NaN))
  expect_error(suggest_keywords(queries = "mtcars", lang = "eng"))
  expect_error(suggest_keywords(queries = "mtcars", lang = 0))
})

test_that("lang param doesn't accept wrong values", {
  expect_error(suggest_keywords(queries = "mtcars", interval = NA))
  expect_error(suggest_keywords(queries = "mtcars", interval = NULL))
  expect_error(suggest_keywords(queries = "mtcars", interval = ""))
  expect_error(suggest_keywords(queries = "mtcars", interval = NaN))
  expect_error(suggest_keywords(queries = "mtcars", interval = "eng"))
  expect_error(suggest_keywords(queries = "mtcars", interval = -1))
})

test_that("lang param doesn't accept wrong values", {
  expect_error(suggest_keywords(queries = "mtcars", interval = NA))
  expect_error(suggest_keywords(queries = "mtcars", interval = NULL))
  expect_error(suggest_keywords(queries = "mtcars", interval = ""))
  expect_error(suggest_keywords(queries = "mtcars", interval = NaN))
  expect_error(suggest_keywords(queries = "mtcars", interval = "eng"))
  expect_error(suggest_keywords(queries = "mtcars", interval = -1))
})

test_that("enhanced param doesn't accept wrong values", {
  expect_error(suggest_keywords(queries = "mtcars", enhanced = NA))
  expect_error(suggest_keywords(queries = "mtcars", enhanced = NULL))
  expect_error(suggest_keywords(queries = "mtcars", enhanced = ""))
  expect_error(suggest_keywords(queries = "mtcars", enhanced = NaN))
  expect_error(suggest_keywords(queries = "mtcars", enhanced = "eng"))
  expect_error(suggest_keywords(query = "mtcars", enhanced = 10))
})

# main ------------------------------------------------------------------------
test_that("MAIN returns some results and waits after the call", {
  start_time <- Sys.time()
  x <- NULL
  x <- googleSuggestQueriesR:::suggest_keywords(queries = "mtcars", interval = 1)
  end_time <- Sys.time()
  diff_time <- difftime(end_time, start_time, units = "secs")

  expect_vector(x)
  expect_gte(length(x), 36)
  expect_gte(diff_time, 36 * 4)
})
