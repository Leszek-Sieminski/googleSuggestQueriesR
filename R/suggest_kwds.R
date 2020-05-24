#' Download suggested keywords for your keywords vector
#'
#' @param queries vector of character strings. The queries that will be used to
#'     create suggestions
#' @param lang character string. The language for which the suggestions will be
#'     created, formatted like: "en", "es", "fr", "de", "pl" etc.
#' @param interval non-negative number. How many seconds must the function wait
#'     after the call to API? Defaults to 0, adjust if exceeded API limits
#'     encountered
#' @param enhanced logical. Do you want to extract more suggestions by adding
#'     letters and digits after input queries? Defaults to TRUE
#'
#' @importFrom utils URLencode
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that
#' @importFrom assertthat not_empty
#'
#' @return vector of keywords suggestion
#' @export
#'
#' @examples
#' \dontrun{
#'   kwds <- suggest_keywords(
#'     queries = c("mtcars", "iris"),
#'     lang = "en",
#'     interval = 1,
#'     enhanced = T
#'   )
#' }
suggest_keywords <- function(queries, lang = "en", interval = 0, enhanced = T)
{
  # params check --------------------------------------------------------------
  assert_that(
    all(!is.na(queries)), all(!is.null(queries)), all(!is.nan(queries)),
    all(not_empty(queries)),
    is.vector(queries, mode = "character"),
    !is.na(lang), !is.null(lang), !is.nan(lang), not_empty(lang),
    length(lang) == 1L, is.character(lang), nchar(lang) == 2L,
    # !is.na(interval), !is.null(interval), !is.nan(interval), not_empty(interval),
    # is.integer(interval), interval >= 0,
    !is.na(enhanced), !is.null(enhanced), !is.nan(enhanced), not_empty(enhanced),
    is.logical(enhanced)
  )

  # download ------------------------------------------------------------------
  if (enhanced) {
    kwds <- unique(unlist(lapply(
      X = create_enhanced_keywords(queries = queries),
      FUN = suggest_single_keyword,
      lang = lang,
      interval = interval
    )))
  } else {
    kwds <- unique(unlist(lapply(
      X = queries,
      FUN = suggest_single_keyword,
      lang = lang,
      interval = interval)))
  }

  # return --------------------------------------------------------------------
  return(kwds)
}
