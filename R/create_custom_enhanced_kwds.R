#' Create additional keywords by creating your own combinations of queries and
#'     choosen vectors.
#'
#' @param queries vector of character strings. 'Bare' queries that will be fed
#'     to the Google Suggest.
#' @param suffix_vec vector of character strings. Optional. It will generate
#'     additional combinations of keywords and prefix_vec contents will be
#'     placed AFTER the queries. Defaults to NULL
#'
#' @return vector of character strings with all combinations of queries and
#'     suffixes/prefixes
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat not_empty
#'
#' @examples
#' \dontrun{
#'   create_custom_enhanced_keywords(
#'     queries = "mtcars",
#'     suffix_vec = letters,
#'     prefix_vec = c(0:9)
#'   )
#' }
create_custom_enhanced_keywords <- function(
  queries,
  suffix_vec = NULL)
{
  # params check --------------------------------------------------------------
  assert_that(
    !any(is.na(queries)), !any(is.null(queries)), !any(is.nan(queries)),
    is.vector(queries, mode = "character"), !any(nchar(queries) < 1),

    is.null(suffix_vec) | is.vector(suffix_vec, mode = "any"),
    all(nchar(suffix_vec) >= 1)
  )

  if (!is.null(suffix_vec)) {
    assert_that(!any(is.na(suffix_vec)), !any(is.nan(suffix_vec)))}


  # create enhanced queries ---------------------------------------------------

  # test_queries <- c("Bristol", "New York", "Warsaw")
  # output_queries <- c()
  #
  # for (i in seq_along(test_queries)) {
  #   output_queries <- c(output_queries, paste(trimws(test_queries[i]), letters))
  # }
  # unique(output_queries)
  # print(output_queries)


  enhanced_queries <- c()

  for (i in seq_along(queries)) {
    enhanced_queries <- c(enhanced_queries, paste(trimws(queries[i]), suffix_vec))
  }

  enhanced_queries <- unique(sort(trimws(enhanced_queries, "right")))

  # enhanced_queries <- unique(sort(c(
  #   paste(trimws(queries), suffix_vec)
  # )))

  # enhanced_queries <- trimws(enhanced_queries, "right")
  return(enhanced_queries)
}
