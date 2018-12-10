#' Identify quotations
#'
#' Identify articles with quotations.
#'
#' @inheritParams e_highlights
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming language"') %>%
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_quotes() -> q
#'
#' library(dplyr)
#'
#' rstats %>%
#'   mutate(quotes = whe_quotes(text)) # pass text column
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{quotes},
#' boolean.
#' If \code{character} vector is passed the function returns a \code{boolean} vector.
#'
#' @rdname whe_quotes
#' @export
whe_quotes <- function(wh) UseMethod("whe_quotes")

#' @rdname whe_quotes
#' @method whe_quotes data.frame
#' @export
whe_quotes.data.frame <- function(wh){
  x <- tokenizers::tokenize_regex(wh$text, '\"')
  x <- sapply(x, length)
  wh$quotes <- as.logical(x - 1)
  wh
}

#' @rdname whe_quotes
#' @method whe_quotes character
#' @export
whe_quotes.character <- function(wh){
  x <- tokenizers::tokenize_regex(wh, '\"')
  x <- sapply(x, length)
  as.logical(x - 1)
}
