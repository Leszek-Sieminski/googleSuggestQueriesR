#' Create additional keywords by creating combinations of queries and letters
#'    and digits.
#'
#' @param queries vector of character strings. 'Bare' queries that will feed
#'     the Google Suggest.
#'
#' @return vector of character strings with all combinations of queries and
#'     alphabet letters and digits
#' @export
#'
#' @examples
#' \dontrun{
#'   create_enhanced_keywords(
#'     queries = c("mtcars", "iris")
#'   )
#' }
create_enhanced_keywords <- function(queries) {
  # params check --------------------------------------------------------------
  assertthat::assert_that(
    !any(is.na(queries)), !is.null(queries), !any(is.nan(queries)),
    assertthat::not_empty(queries), is.vector(queries, mode = "character"),
    all(nchar(queries) >= 1))

  # create enhanced queries ---------------------------------------------------
    enhanced_queries <- sort(
      c(paste0(queries, " ", letters),
        paste0(queries, " ", c(0:9)))
    )

  return(enhanced_queries)
}
