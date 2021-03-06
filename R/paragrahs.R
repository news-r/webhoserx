#' Get number of paragraphs
#'
#' Counts number of paragraphs in text.
#'
#' @inheritParams e_highlights
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming" OR Python') %>%  # use highlight!
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_paragraphs() -> nparagraphs
#'
#' library(dplyr)
#'
#' rstats %>%
#'   mutate(nparagraphs = whe_paragraphs(text)) -> paragraphs
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{nparagraphs}.
#' If \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @rdname whe_paragraphs
#' @export
whe_paragraphs <- function(wh) UseMethod("whe_paragraphs")

#' @rdname whe_paragraphs
#' @method whe_paragraphs data.frame
#' @export
whe_paragraphs.data.frame <- function(wh){
  para <- sapply(wh$text, function(x){
    length(tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")[[1]])
  })

  wh$nparagraphs <- unname(para) # unname

  wh
}

#' @rdname whe_paragraphs
#' @method whe_paragraphs character
#' @export
whe_paragraphs.character <- function(wh){
  sapply(wh, function(x){
    length(tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")[[1]])
  })
}
