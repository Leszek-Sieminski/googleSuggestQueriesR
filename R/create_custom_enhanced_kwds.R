#' Create additional keywords by creating your own combinations of queries and
#'     choosen vectors.
#'
#' @param queries vector of character strings. 'Bare' queries that will feed
#'     the Google Suggest.
#' @param suffix_vec vector of character strings. Optional. It will generate
#'     additional combinations of keywords and prefix_vec contents will be
#'     placed AFTER the queries. Defaults to NULL
#' @param prefix_vec vector of character strings. Optional. It will generate
#'     additional combinations of keywords and prefix_vec contents will be
#'     placed BEFORE the queries. Defaults to NULL.
#'
#' @return vector of character strings with all combinations of queries and
#'     suffixes/prefixes
#' @export
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
  assertthat::assert_that(
    !is.na(queries), !is.null(queries), !is.nan(queries), assertthat::not_empty(queries),
    is.vector(queries, mode = "character"),
    is.null(suffix_vec) | is.vector(suffix_vec, mode = "any"))

  # create enhanced queries ---------------------------------------------------
  enhanced_queries <- unique(sort(
    c(paste0(queries, " ", suffix_vec))
  ))

  return(enhanced_queries)
}
