#' Get highlighted terms
#'
#' Extract highlighted terms
#'
#' @param wh \emph{highlighted} object returned by \link[webhoser]\code{wh_collect}, see examples.
#' @param flatten whether to flatten ngrams.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming language"') %>%  # use highlight!
#'   wh_collect() -> wef # collect results
#'
#' wef %>%
#'   whe_highlights() -> highlights
#'
#' highlights <- wef %>%
#'   dplyr::mutate(
#'     nparagraphs = whe_highlights(text)
#'   )
#' }
#'
#' @rdname whe_highlights
#' @export
whe_highlights <- function(wh, flatten = FALSE) UseMethod("whe_highlights")

#' @rdname whe_highlights
#' @method whe_highlights data.frame
whe_highlights.data.frame <- function(wh, flatten = FALSE){
  x <- lapply(stringr::str_extract_all(wh$highlightText, "<em>(.*?)</em>"), function(x){
    gsub("<em>|</em>", "", x)
  })

  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  wh$highlights <- x
  wh
}

#' @rdname whe_highlights
#' @method whe_highlights character
whe_highlights.character <- function(wh, flatten = FALSE){
  x <- lapply(stringr::str_extract_all(wh$highlightText, "<em>(.*?)</em>"), function(x){
    gsub("<em>|</em>", "", x)
  })

  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  x
}
