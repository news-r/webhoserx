#' Get number of paragraphs with mentions
#'
#' Counts number of paragraphs with mentions.
#'
#' @inheritParams e_highlights
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = 'R OR Python', highlight = TRUE) %>%  # use highlight!
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_mentioned() -> mentioned
#'
#' mentioned <- rstats %>%
#'   dplyr::mutate(nmentioned = whe_mentioned(highlightText))
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{mentioned}.
#' If \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @rdname whe_mentioned
#' @export
whe_mentioned <- function(wh) UseMethod("whe_mentioned")

#' @rdname whe_mentioned
#' @method whe_mentioned data.frame
#' @export
whe_mentioned.data.frame <- function(wh){
  para <- lapply(wh$highlightText, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")
  })

  mentions <- lapply(para, function(x){
    lapply(x, function(y){
      stringr::str_extract_all(y, "<em>(.*?)</em>")
    })
  })

  x <- lapply(mentions, function(x){
    lapply(x, function(y){
      sapply(y, length)
    })
  })

  wh$mentioned <- sapply(x, function(z){
    length(z[[1]][z[[1]] > 0])
  })
  wh
}

#' @rdname whe_mentioned
#' @method whe_mentioned character
#' @export
whe_mentioned.character <- function(wh){
  para <- lapply(wh, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")
  })

  mentions <- lapply(para, function(x){
    lapply(x, function(y){
      stringr::str_extract_all(y, "<em>(.*?)</em>")
    })
  })

  x <- lapply(mentions, function(x){
    lapply(x, function(y){
      sapply(y, length)
    })
  })

  sapply(x, function(z){
    length(z[[1]][z[[1]] > 0])
  })
}
