#' Sentiment
#'
#' Get Sentiment score from text.
#'
#' @inheritParams e_highlights
#' @param method Sentiment method passed to \link[syuzhet]{get_sentiment}.
#' @param ... Any other parameters to pass to \link[syuzhet]{get_sentiment}.
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
#' sent <- rstats %>%
#'   whe_sentiment()
#'
#' library(dplyr)
#'
#' rstats %>%
#'   mutate(sent = whe_sentiment(text))
#' }
#'
#' @export
whe_sentiment <- function(wh, method = "afinn", ...) UseMethod("whe_sentiment")

#' @export
whe_sentiment.character <- function(wh, method = "afinn", ...){
  syuzhet::get_sentiment(wh, method = method, ...)
}

#' @export
whe_sentiment.character <- function(wh, method = "afinn", ...){
  wh[["sentiment"]] <- syuzhet::get_sentiment(wh[["text"]], method = method, ...)
  wh
}
