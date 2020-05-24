context("Custom enhanced")
library(googleSuggestQueriesR)
library(testthat)

# defensive -------------------------------------------------------------------
test_that("queries param doesn't accept wrong values", {
  expect_error(create_custom_enhanced_keywords(queries = NA))
  expect_error(create_custom_enhanced_keywords(queries = NULL))
  expect_error(create_custom_enhanced_keywords(queries = ""))
  expect_error(create_custom_enhanced_keywords(queries = NaN))
  expect_error(create_custom_enhanced_keywords(queries = 0))
})

test_that("suffix_vec param doesn't accept wrong values", {
  expect_error(create_custom_enhanced_keywords(queries = "mtcars", suffix_vec = NA))
  expect_error(create_custom_enhanced_keywords(queries = "mtcars", suffix_vec = ""))
  expect_error(create_custom_enhanced_keywords(queries = "mtcars", suffix_vec = NaN))
})

# main ------------------------------------------------------------------------
test_that("vector + NULL output is OK", {
  x <- create_custom_enhanced_keywords("mtcars", NULL)
  y <- c("mtcars")
  expect_identical(length(x), length(y), 26)
})

test_that("vector + prefix output is OK", {
  x <- create_custom_enhanced_keywords("mtcars", letters)
  y <- c(
    "mtcars a", "mtcars b", "mtcars c", "mtcars d", "mtcars e", "mtcars f",
    "mtcars g", "mtcars h", "mtcars i", "mtcars j", "mtcars k", "mtcars l",
    "mtcars m", "mtcars n", "mtcars o", "mtcars p", "mtcars q", "mtcars r",
    "mtcars s", "mtcars t", "mtcars u", "mtcars v", "mtcars w", "mtcars x",
    "mtcars y", "mtcars z")
  expect_equal(length(x), length(y))
  expect_identical(x, y)
})
