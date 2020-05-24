context("Basic enhanced")
library(googleSuggestQueriesR)
library(testthat)

# defensive -------------------------------------------------------------------
test_that("queries param doesn't accept wrong values", {
  expect_error(create_enhanced_keywords(queries = NA))
  expect_error(create_enhanced_keywords(queries = NULL))
  expect_error(create_enhanced_keywords(queries = ""))
  expect_error(create_enhanced_keywords(queries = NaN))
  expect_error(create_enhanced_keywords(queries = 0))
})

# main ------------------------------------------------------------------------
test_that("vector + NULL output is OK", {
  x <- create_enhanced_keywords("mtcars")
  expect_equal(length(x), 36)
})

test_that("vector + prefix output is OK", {
  x <- create_enhanced_keywords("mtcars")
  y <- c("mtcars 0", "mtcars 1", "mtcars 2", "mtcars 3", "mtcars 4", "mtcars 5",
         "mtcars 6", "mtcars 7", "mtcars 8", "mtcars 9", "mtcars a", "mtcars b",
         "mtcars c", "mtcars d", "mtcars e", "mtcars f", "mtcars g", "mtcars h",
         "mtcars i", "mtcars j", "mtcars k", "mtcars l", "mtcars m", "mtcars n",
         "mtcars o", "mtcars p", "mtcars q", "mtcars r", "mtcars s", "mtcars t",
         "mtcars u", "mtcars v", "mtcars w", "mtcars x", "mtcars y", "mtcars z")
  expect_equal(length(x), length(y))
  expect_identical(x, y)
})
