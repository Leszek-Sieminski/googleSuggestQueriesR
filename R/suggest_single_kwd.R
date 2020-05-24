#' Download suggested keywords for a single input query
#'
#' @param query character string. The query that will be used to create suggestions
#' @param lang character string. The language for which the suggestions will be
#'     created, formatted like: "en", "es", "fr", "de", "pl" etc.
#' @param interval non-negative number. How many seconds must the function wait
#'     after the call to API? Defaults to 0, adjust if exceeded API limits
#'     encountered
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that
#' @importFrom assertthat not_empty
#'
#' @return single column data frame with suggested queries
#'
#' @examples
#' \dontrun{
#'   kwds <- suggest_single_keyword(
#'     queries = "mtcars",
#'     lang = "en",
#'     interval = 1
#'   )
#' }
suggest_single_keyword <- function(query,
                                 lang = "en",
                                 interval = 0){
  # params check --------------------------------------------------------------
  assert_that(
    !is.na(query), !is.null(query), !is.nan(query), not_empty(query),
    length(query) == 1L, is.character(query),
    !is.na(lang), !is.null(lang), !is.nan(lang), not_empty(lang),
    length(lang) == 1L, is.character(lang), nchar(lang) == 2L,
    !is.na(interval), !is.null(interval), !is.nan(interval),
    interval >= 0
  )

  # params prep ---------------------------------------------------------------
  keyword <- URLencode(query) # to safely encode keyword for URLs (spaces,
                              # special signs, other chars)

  # query preparation ---------------------------------------------------------
  prepared_query <- paste0(
    "http://suggestqueries.google.com/complete/search",
    "?output=", "firefox", # setting "firefox" forces returning of JSON
    "&hl=", lang,
    "&q=", keyword)

  # download the data ---------------------------------------------------------
  data_object <- fromJSON(content(GET(prepared_query), "text")) # parsing
  data_object <- data_object[[2]]

  # pause to not exceed API limits and return ---------------------------------
  Sys.sleep(3 + interval) # there are undocumented API limits that may break
                          # the process if exceeded

  return(data_object)
}
