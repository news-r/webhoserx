#' Get number of sentences
#'
#' Counts number of sentences in text.
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
#'   whe_sentences() -> nsentences
#'
#' library(dplyr)
#' rstats %>%
#'   mutate(nsentences = whe_sentences(text)) -> nsentences
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{nsentences}.
#' If \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @rdname whe_sentences
#' @export
whe_sentences <- function(wh) UseMethod("whe_sentences")

#' @rdname whe_sentences
#' @method whe_sentences data.frame
#' @export
whe_sentences.data.frame <- function(wh){
  wh$nsentences <- sapply(wh$text, function(x){
    length(tokenizers::tokenize_sentences(x)[[1]])
  })
  wh
}

#' @rdname whe_sentences
#' @method whe_sentences character
#' @export
whe_sentences.character <- function(wh){
  sapply(wh, function(x){
    length(tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")[[1]])
  })
}
