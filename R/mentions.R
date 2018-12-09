#' Get number of mentions
#'
#' Counts number of mentions in highlighted text.
#'
#' @param wh \emph{highlighted} object returned by \code{wh_collect}, see examples.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"World Economic Forum"') %>%  # use highlight!
#'   wh_collect() -> wef # collect results
#'
#' wef %>%
#'   whe_mentions() -> nmentions
#'
#' library(dplyr)
#' wef %>%
#'   mutate(nmentions = whe_mentions(text)) -> mentions
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{nmentions}.
#' If \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @rdname whe_mentions
#' @export
whe_mentions <- function(wh) UseMethod("whe_mentions")

#' @rdname whe_mentions
#' @method whe_mentions data.frame
#' @export
whe_mentions.data.frame <- function(wh){
  wh$nmentions <- sapply(wh$highlightText, function(x){
    length(tokenizers::tokenize_regex(x, pattern = "<em>")[[1]]) - 1
  })
  wh
}

#' @rdname whe_mentions
#' @method whe_mentions character
#' @export
whe_mentions.character <- function(wh){
  sapply(wh, function(x){
    length(tokenizers::tokenize_regex(x, pattern = "<em>")[[1]]) - 1
  })
}
