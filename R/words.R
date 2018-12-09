#' Get number of words
#'
#' Counts number of words in text.
#'
#' @inheritParams e_highlights
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming"') %>%
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_words() -> nwords
#'
#' library(dplyr)
#' rstats %>%
#'   mutate(nwords = whe_words(text)) -> words
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{nwords}.
#' If \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @rdname whe_words
#' @export
whe_words <- function(wh) UseMethod("whe_words")

#' @rdname whe_words
#' @method whe_words data.frame
#' @export
whe_words.data.frame <- function(wh){
  wh$nwords <- sapply(wh$text, function(x){
    length(tokenizers::tokenize_words(x)[[1]])
  })
  wh
}

#' @rdname whe_words
#' @method whe_words character
#' @export
whe_words.character <- function(wh){
  sapply(wh, function(x){
    length(tokenizers::tokenize_words(x)[[1]])
  })
}
